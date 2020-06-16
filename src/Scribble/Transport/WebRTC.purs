module Scribble.Transport.WebRTC where

import Scribble.Transport

import Web.Socket.WebSocket as WS
import Web.Event.EventTarget (eventListener, addEventListener, EventListener(..))
import Web.Socket.Event.MessageEvent as ME
import Web.Socket.Event.EventTypes as WSET

import Effect.Class (liftEffect)
import Effect (Effect)
import Effect.Aff.AVar (AVar, new, empty, put, read, take, tryTake, tryRead, tryPut)
import Effect.Aff (Aff, launchAff, launchAff_, delay)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)

import Control.Monad.Except (runExcept)
import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Foreign (F, Foreign, unsafeToForeign, readString)
import Effect.Exception (error)
import Control.Monad.Error.Class (throwError)

import Data.Maybe (Maybe(..))
import Prelude (Unit, const, pure, unit, ($), (<<<), bind, discard, void, (>>=), (>>>), flip, (<>), (==), otherwise)

import Data.Time.Duration
import Data.List (List(..))

import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Combinators
import Data.Argonaut.Decode.Combinators
import Data.Argonaut.Core (Json)
import Data.Generic.Rep (class Generic)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Core (stringify, jsonEmptyObject)
import Data.Either(fromLeft, fromRight)
import WebRTC.RTC
import WebRTC.RTC as RTC

data Status = AnswerReceived | AnswerSent
data DataChanStatus = Open | Closed
data RTCConnectionStatus = RTCConnected | RTCDisconnected | RTCFailed | RTCClosed
data URL = URL String
data WebSocket = WebSocket (AVar Status) (AVar Json) WS.WebSocket
data WebRTCConnection = WebRTCConnection RTCDataChannel RTCPeerConnection (AVar DataChanStatus) (AVar Json)

newtype SDPMessage = SDPMessage {type :: String, sender :: String, receipient :: String, otherParams :: Maybe String, content :: String}
instance decodeConnectInfo :: DecodeJson SDPMessage where
  decodeJson json = do
    obj <- decodeJson json
    t <- obj .: "type"
    sender <- obj .: "sender"
    receipient <- obj .: "receipient"
    otherParams <- obj .: "otherParams"
    content <- obj .: "content"
    pure $ SDPMessage { "type":t, "sender":sender, "receipient":receipient, "otherParams":otherParams, "content":content}    
instance encodeConnectInfo :: EncodeJson SDPMessage where
  encodeJson (SDPMessage { "type":t, "sender":sender, "receipient":receipient, "otherParams":otherParams, "content":content }) =
      "type" := t
      ~> "sender" := sender
      ~> "receipient" := receipient
      ~> "otherParams" := encodeJson otherParams
      ~> "content" := content
      ~> jsonEmptyObject

type RTCConnectionInfo = {thisPeerId :: String, remotePeerId :: String}

modifyVar :: forall a. (a -> a) -> AVar a -> Aff Unit
modifyVar f v = do
  x <- take v
  put (f x) v

updateVar :: forall a. a -> AVar a -> Aff Unit
updateVar a v = do
  r <- tryPut a v
  case r of
    true -> pure unit
    false -> do
      _ <- take v
      updateVar a v

createSDPMessageString :: forall a. SDPMessage -> Aff String
createSDPMessageString msg = do
  pure $ (encodeJson >>> stringify) msg

createOfferString :: forall e. RTCSessionDescription -> Aff String
createOfferString offer = do
  pure $ (encodeJson >>> stringify) offer

createAnswerString :: forall e. RTCSessionDescription -> Aff String
createAnswerString answer = do
  pure $ (encodeJson >>> stringify) answer

readDescription :: forall e. String -> Either String RTCSessionDescription
readDescription s = (jsonParser s) >>= decodeJson

readIceCandidate :: forall e. String -> Either String RTCIceCandidate
readIceCandidate s = (jsonParser s) >>= decodeJson

readSDPMessage :: forall e. String -> Either String SDPMessage
readSDPMessage s = (jsonParser s) >>= decodeJson

sendOffer :: RTCPeerConnection -> WS.WebSocket -> RTCConnectionInfo -> Aff Unit
sendOffer conn socket connInfo = do
  offer <- createOffer conn
  offerStr <- createOfferString offer
  msgStr <- createSDPMessageString (SDPMessage {"type":"offer", "sender": connInfo.thisPeerId, "receipient": connInfo.remotePeerId, "otherParams": Nothing, "content": offerStr})
  liftEffect $ WS.sendString socket msgStr
  setLocalDescription offer conn

handleAnswer :: RTCPeerConnection -> RTCSessionDescription -> Aff Unit
handleAnswer conn answer = do
  answerStr <- createAnswerString answer
  liftEffect $ log ("Received answer: " <> answerStr)
  setRemoteDescription answer conn

readSDPAnswer :: RTCPeerConnection -> String -> SDPMessage -> Aff Unit
readSDPAnswer conn offerReceipient (SDPMessage msg)
  |msg.type == "answer", msg.sender == offerReceipient = either (\e -> pure unit) (handleAnswer conn) (readDescription msg.content)
  |msg.type == "ice", msg.sender == offerReceipient =  either (\e -> pure unit) (liftEffect <<< ((flip addIceCandidate) conn)) (readIceCandidate msg.content)
  |otherwise = pure unit

receiveAnswerListener :: RTCPeerConnection -> String -> Effect EventListener
receiveAnswerListener conn offerRecepient = eventListener \ev -> do
  for_ (ME.fromEvent ev) \msgEvent ->
    for_ (readHelper readString (ME.data_ msgEvent)) \msg ->
      either (\e -> pure unit) (void <<< launchAff <<< readSDPAnswer conn offerRecepient) (readSDPMessage msg)

onIceCandidateHandler :: WS.WebSocket -> RTCConnectionInfo -> (IceEvent -> Effect Unit)
onIceCandidateHandler socket connInfo = \ev -> launchAff_ $ do
  case (iceEventCandidate ev) of
    Nothing -> pure unit
    Just c -> do
      msgStr <- createSDPMessageString (SDPMessage {"type": "ice", "sender": connInfo.thisPeerId, "receipient": connInfo.remotePeerId, "otherParams": Nothing, "content": (stringify $ encodeJson c)})
      liftEffect $ WS.sendString socket msgStr

onConnStateHandler :: AVar RTCConnectionStatus -> (String -> Effect Unit)
onConnStateHandler buf = \s -> launchAff_ $ do
  case s of
    "connected" -> updateVar RTCConnected buf
    "failed" -> updateVar RTCFailed buf
    "disconnected" -> updateVar RTCDisconnected buf
    "closed" -> updateVar RTCClosed buf
    _ -> pure unit

readConnStatus :: AVar RTCConnectionStatus -> Aff Unit
readConnStatus v = do
  cs <- read v
  case cs of
    RTCConnected -> do
      pure unit
    _ -> (delay (Milliseconds 200.0)) <> readConnStatus v

readDataChanStatus :: AVar DataChanStatus-> Aff Unit
readDataChanStatus v = do
  cs <- read v
  case cs of
    Open -> do
      pure unit
    _ -> (delay (Milliseconds 200.0)) <> readDataChanStatus v


-- connectInfo is for sending extra info needed for creating the connection
connect :: forall a. URL -> Maybe String -> RTCConnectionInfo -> Aff WebRTCConnection
connect (URL url) connectInfo pInfo@{thisPeerId: thisP, remotePeerId: remoteP} = do
  conn <- liftEffect $ newRTCPeerConnection defaultRTCConfiguration
  socket <- liftEffect $ WS.create url []
  liftEffect $ do
    el <- (eventListener \_ -> void $ launchAff $ do
      log "open"
      case connectInfo of
        Nothing -> pure unit
        Just s -> liftEffect $ WS.sendString socket s
      sendOffer conn socket pInfo)
    addEventListener
      WSET.onOpen
      el
      false
      (WS.toEventTarget socket)
  liftEffect $ do
    el <- receiveAnswerListener conn remoteP
    addEventListener
      WSET.onMessage
      el
      false
      (WS.toEventTarget socket)
  
  recvBuf <- empty
  dataChanStatus <- empty
  connStatus <- empty
  dataChannel <- liftEffect $ createDataChannel "dataChannel" conn
  liftEffect $ onicecandidate (onIceCandidateHandler socket pInfo) conn
  liftEffect $ onconnectionstatechange (onConnStateHandler connStatus) conn
  liftEffect $ do
    onopen (launchAff_ $ put Open dataChanStatus) dataChannel
    onmessage (\msg -> launchAff_ $ either (\e -> pure unit) ((flip put) recvBuf) (jsonParser msg)) dataChannel
    ondatachannelclose (launchAff_ $ put Closed dataChanStatus) dataChannel
  _ <- readConnStatus connStatus
  _ <- readDataChanStatus dataChanStatus
  -- liftEffect $ WS.close socket
  pure (WebRTCConnection dataChannel conn dataChanStatus recvBuf)

sendAnswer ::RTCPeerConnection -> WS.WebSocket -> RTCConnectionInfo -> RTCSessionDescription -> Aff Unit
sendAnswer conn socket connInfo sdp = do
  setRemoteDescription sdp conn
  answer <- createAnswer conn
  answerStr <- createAnswerString answer
  setLocalDescription answer conn
  answerStr <- createSDPMessageString (SDPMessage {"type": "answer", "sender": connInfo.thisPeerId, "receipient": connInfo.remotePeerId, "otherParams": Nothing, "content": answerStr})
  liftEffect $ WS.sendString socket answerStr

readHelper :: forall a b. (Foreign -> F a) -> b -> Maybe a
readHelper read =
  either (const Nothing) Just <<< runExcept <<< read <<< unsafeToForeign

readSDPOffer :: RTCPeerConnection -> WS.WebSocket -> RTCConnectionInfo -> SDPMessage -> Aff Unit
readSDPOffer conn socket connInfo (SDPMessage msg)
  |msg.type == "offer", msg.sender == connInfo.remotePeerId = either (\e -> pure unit) (sendAnswer conn socket connInfo) (readDescription msg.content)
  |msg.type == "ice", msg.sender == connInfo.remotePeerId =  either (\e -> pure unit) (liftEffect <<< (flip addIceCandidate) conn) (readIceCandidate msg.content)
  |otherwise = pure unit

receiveOfferListener :: RTCPeerConnection -> WS.WebSocket -> RTCConnectionInfo -> Effect EventListener
receiveOfferListener conn socket connInfo = eventListener \ev -> do
  for_ (ME.fromEvent ev) \msgEvent ->
    for_ (readHelper readString (ME.data_ msgEvent)) \msg ->
      either (\e -> pure unit) (launchAff_ <<< readSDPOffer conn socket connInfo) ((jsonParser msg) >>= decodeJson) 

serve :: forall a.URL -> Maybe String -> RTCConnectionInfo -> Aff WebRTCConnection
serve (URL url) serveInfo pInfo@{thisPeerId: thisP, remotePeerId: remoteP} = do
  conn <- liftEffect $ newRTCPeerConnection defaultRTCConfiguration
  socket <- liftEffect $ WS.create url []
  liftEffect $ do
    el <- (eventListener \_ -> void $ launchAff $ do
      log "open"
      case serveInfo of
        Nothing -> pure unit
        Just s -> liftEffect $ WS.sendString socket s)
    addEventListener
      WSET.onOpen
      el
      false
      (WS.toEventTarget socket)
  liftEffect $ do
    el <- receiveOfferListener conn socket pInfo
    addEventListener
      WSET.onMessage
      el
      false
      (WS.toEventTarget socket)
  dataChanVar <- empty
  recvBuf <- empty
  dataChanStatus <- empty
  connStatus <- empty
  liftEffect $ ondatachannel (dcHandler dataChanVar recvBuf dataChanStatus) conn
  liftEffect $ onicecandidate (onIceCandidateHandler socket pInfo) conn
  liftEffect $ onconnectionstatechange (onConnStateHandler connStatus) conn
  ibuf <- empty
  dataChannel <- read dataChanVar
  _ <- readConnStatus connStatus
  -- liftEffect $ WS.close socket
  pure (WebRTCConnection dataChannel conn dataChanStatus recvBuf)
  where
    dcHandler dataChanVar recvBuf dataChanStatus = \datachannel -> do 
      launchAff_ $ put datachannel dataChanVar
      onmessage (\msg -> launchAff_ $ either (\e -> pure unit) ((flip put) recvBuf) (jsonParser msg)) datachannel
      ondatachannelclose (launchAff_ $ put Closed dataChanStatus) datachannel
      onopen (launchAff_ $ put Open dataChanStatus) datachannel

send :: WebRTCConnection -> Json -> Aff Unit
send (WebRTCConnection dc _ sv _) msg = do
  status <- read sv
  case status of
   Open -> liftEffect $ RTC.send (stringify msg) dc
   Closed -> pure unit

receive :: WebRTCConnection -> Aff Json
receive (WebRTCConnection _ _ sv buf) = do
  status <- read sv
  case status of
    Open -> take buf 
    Closed -> do
      -- The socket is closed, but there might be unprocessed input still
      x <- tryTake buf
      case x of
        Nothing -> throwError $ error "Channel is closed"
        Just val -> pure val
  
close :: WebRTCConnection -> Aff Unit
close (WebRTCConnection _ pc _ _) = do
  liftEffect $ closeRTCPeerConnection pc

instance webRTCURLTransport :: Transport WebRTCConnection URL where
  send = \ws -> liftAff <<< (send ws)
  receive = liftAff <<< receive
  close = liftAff <<< close

instance webRTCURLTransportClient :: TransportClient WebRTCConnection URL ("loginMsg" :: Maybe String, "connInfo" :: {thisPeerId :: String, remotePeerId :: String}) where
  connect p x = liftAff $ connect p x.loginMsg x.connInfo

instance webRTCURLTransportServe :: TransportServer WebRTCConnection URL ("loginMsg" :: Maybe String, "connInfo" :: {thisPeerId :: String, remotePeerId :: String}) where
  serve p x = liftAff $ serve p x.loginMsg x.connInfo
