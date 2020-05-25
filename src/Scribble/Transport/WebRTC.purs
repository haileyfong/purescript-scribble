module Scribble.Transport.WebRTC where

import Scribble.Transport

import Web.Socket.WebSocket as WS
import Web.Event.EventTarget as EET
import Web.Socket.Event.MessageEvent as ME
import Web.Socket.Event.EventTypes as WSET

import Effect.Class (liftEffect)
import Effect.Aff.AVar (AVar, new, empty, put, read, take, tryTake)
import Effect.Aff (Aff, launchAff)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)

import Control.Monad.Except (runExcept)
import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Foreign (F, Foreign, unsafeToForeign, readString)

import Data.Maybe (Maybe(..))
import Prelude (Unit, const, pure, unit, ($), (<<<), bind, discard, void, (>>=), (>>>), flip)

import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Core (Json)
import Data.Generic.Rep (class Generic)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Core (stringify)

import WebRTC.RTC (defaultRTCConfiguration, newRTCPeerConnection, RTCPeerConnection(..), createDataChannel, createOffer, createAnswer, setLocalDescription, setRemoteDescription)


data URL = URL String

data SDP = Offer | Answer
derive instance repGenericSDP :: Generic SDP _
instance decodeSDP :: Decode SDP where
  decode = genericDecodeEnum { constructorTagTransform: id }
instance encodeSDP :: Encode SDP where
  encode = genericEncodeEnum { constructorTagTransform: id }

newtype SdpInfo = SdpInfo
  { type :: SDP
  , sdp :: String
  }
derive instance repGenericSdpInfo :: Generic SdpInfo _
instance decodeSdpInfo :: Decode SdpInfo where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeSdpInfo :: Encode SdpInfo where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

createOfferString :: forall e. RTCSessionDescription -> Aff String
createOfferString offer = do
  pure $ (encodeJson >>> stringify) offer

createAnswerString :: forall e. RTCSessionDescription -> Aff String
createAnswerString answer = do
  pure $ (encodeJson >>> stringify) answer

readDescription :: forall e. String -> Aff (Either String RTCSessionDescription)
readDescription s = pure $ (jsonParser s) >>= decodeJson

readHelper :: forall a b. (Foreign -> F a) -> b -> Maybe a
readHelper read = either (const Nothing) Just <<< runExcept <<< read <<< unsafeToForeign

receiveListener :: RTCPeerConnection -> Effect eventListener
receiveListener conn = EET.eventListener \ev -> do
  for_ (ME.fromEvent ev) \msgEvent ->
    for_ (readHelper readString (ME.data_ msgEvent)) \msg -> do
      res <- (runExcept <<< decodeJSON) (jsonParser msg)
      setRemoteDescription $ readDescription res.sdp
      case res.type of
        Answer -> pure conn
        Offer -> pure unit

connect :: URL -> Aff RTCPeerConnection
connect (URL url) = do
  conn <- liftEffect $ newRTCPeerConnection defaultRTCConfiguration
  socket <- liftEffect $ WS.create url []
  offer <- createOffer conn
  setLocalDescription offer conn
  offerString <- createOfferString offer
  json <- encodeJson $ SdpInfo{type: Offer, sdp: offerString}
  WS.sendString socket $ stringify json
  liftEffect $ do
    EET.addEventListener
      WSET.onMessage
      (receiveListener conn)
      false
      (WS.toEventTarget socket)
  liftEffect $ do
    el <- (EET.eventListener \_ -> void $ launchAff $ do
        liftEffect $ log "open"
        -- put Open status
    EET.addEventListener
      WSET.onOpen
      el
      false
      (WS.toEventTarget socket)
  pure conn

receiveListenerServe :: RTCPeerConnection -> Effect eventListener
receiveListener conn = EET.eventListener \ev -> do
  for_ (ME.fromEvent ev) \msgEvent ->
    for_ (readHelper readString (ME.data_ msgEvent)) \msg -> do
      res <- (runExcept <<< decodeJSON) (jsonParser msg)
      setRemoteDescription $ readDescription res.sdp
        case res.type of
          Answer -> pure unit
          Offer -> do 
            answer <- createAnswer conn
            setLocalDescription answer
            answerString <- createAnswerString answer
            json <- encodeJson $ SdpInfo{type: Answer, sdp: answerString}
            WS.sendString socket $ stringify json
            pure conn

serve :: URL -> Aff RTCPeerConnection
serve (URL url) = do
  conn <- liftEffect $ newRTCPeerConnection defaultRTCConfiguration
  socket <- liftEffect $ WS.create url []
  liftEffect $ do
    EET.addEventListener
      WSET.onMessage
      (receiveListenerServe conn)
      false
      (WS.toEventTarget socket)
  liftEffect $ do
    el <- (EET.eventListener \_ -> void $ launchAff $ do
        liftEffect $ log "open"
        -- put Open status
    EET.addEventListener
      WSET.onOpen
      el
      false
      (WS.toEventTarget socket)
  pure conn


  
-- connect :: URL -> Aff RTCPeerConnection
-- connect (URL url) = do
--   conn <- liftEffect $ newRTCPeerConnection defaultRTCConfiguration
--   socket <- liftEffect $ WS.create url []
--   -- Add the listener for the connection opening
--   liftEffect $ do
--     el <- (EET.eventListener \_ -> void $ launchAff $ do
--         liftEffect $ log "open"
--         -- put Open status
--     EET.addEventListener
--       WSET.onOpen
--       el
--       false
--       (WS.toEventTarget socket)
--   -- Add the listener for receiving messages
--   ibuf <- empty
--   liftEffect $ do
--     el <- (receiveListener ibuf)
--     EET.addEventListener
--       WSET.onMessage
--       el
--       false
--       (WS.toEventTarget socket)
--    -- Send offer
--    offer <- createOffer conn
--    setLocalDescription offer conn
--    offerString <- createOfferString offer
--    json <- encodeJson $ SdpInfo{type: Offer, sdp: offerString}
--    WS.sendString socket $ stringify json
-- --    liftEffect $ WS.close socket
--    pure conn
--    where
--     receiveListener ibuf = EET.eventListener \ev -> do
--       for_ (ME.fromEvent ev) \msgEvent ->
--         for_ (readHelper readString (ME.data_ msgEvent)) \msg ->
--           res <- (runExcept <<< decodeJSON) (jsonParser msg)
--           setRemoteDescription $ readDescription res.sdp
--           case res.type of
--             Answer -> pure conn
--             Offer -> do 
--               answer <- createAnswer conn
--               setLocalDescription answer
--               answerString <- createAnswerString answer
--               json <- encodeJson $ SdpInfo{type: Answer, sdp: answerString}
--               WS.sendString socket $ stringify json
--               pure conn
--         --   either (\e -> pure unit) (void <<< launchAff <<< (flip put) ibuf) (jsonParser msg)
--     readHelper :: forall a b. (Foreign -> F a) -> b -> Maybe a
--     readHelper read =
--       either (const Nothing) Just <<< runExcept <<< read <<< unsafeToForeign

-- send :: RTCPeerConnection -> Json -> Aff Unit


-- receive :: WebSocket -> Aff Json
-- close :: WebSocket -> Aff Unit

instance rtcURLTransport :: Transport RTCPeerConnection URL where
  -- TODO: Make pointfree
  send = \ws -> liftAff <<< (send ws)
  receive = liftAff <<< receive
  close = liftAff <<< close

instance rtcURLTransportClient :: TransportClient RTCPeerConnection URL where
  connect = liftAff <<< connect