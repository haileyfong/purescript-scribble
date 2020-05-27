module Scribble.Transport.WebRTC where

import Scribble.Transport

import Web.Socket.WebSocket as WS
import Web.Event.EventTarget (eventListener, addEventListener, EventListener(..))
import Web.Socket.Event.MessageEvent as ME
import Web.Socket.Event.EventTypes as WSET

import Effect.Class (liftEffect)
import Effect (Effect)
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
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Combinators
import Data.Argonaut.Decode.Combinators
import Data.Argonaut.Core (Json)
import Data.Generic.Rep (class Generic)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Core (stringify, jsonEmptyObject)

import WebRTC.RTC (defaultRTCConfiguration, newRTCPeerConnection, RTCPeerConnection(..), createDataChannel, createOffer, createAnswer, setLocalDescription, setRemoteDescription, RTCSessionDescription(..))
import WebRTC.RTC

data Status = Open | Closed
data URL = URL String 
data WebSocket = WebSocket (AVar Status) (AVar Json) WS.WebSocket

-- data SDP = Offer | Answer 
-- derive instance repGenericSDP :: Generic SDP _
-- instance decodeSDP :: DecodeJson SDP where
--   decodeJson = genericDecodeJson
-- instance encodeSDP :: EncodeJson SDP where
--   encodeJson = genericEncodeJson


-- typeToString :: SDP -> String
-- typeToString t = case t of 
--   Offer -> "offer"
--   Answer -> "answer"

-- newtype SdpInfo = SdpInfo
--   { t :: SDP
--   , sdp :: String
--   }
-- derive instance repGenericSdpInfo :: Generic SdpInfo _
-- instance decodeSdpInfo :: DecodeJson SdpInfo where
--   decodeJson json = do
--     obj <- decodeJson json
--     t <- obj .: "type"
--     sdp <- obj .: "sdp"
--     pure $ SdpInfo { t, sdp}    
-- instance encodeSdpInfo :: EncodeJson SdpInfo where
--   encodeJson (SdpInfo { t, sdp }) =
--     "type" := typeToString t
--       ~> "sdp" := sdp
--       ~> jsonEmptyObject

createOfferString :: forall e. RTCSessionDescription -> Aff String
createOfferString offer = do
  pure $ (encodeJson >>> stringify) offer

createAnswerString :: forall e. RTCSessionDescription -> Aff String
createAnswerString answer = do
  pure $ (encodeJson >>> stringify) answer

readDescription :: forall e. String -> Aff (Either String RTCSessionDescription)
readDescription s = pure $ (jsonParser s) >>= decodeJson

sendOffer :: RTCPeerConnection -> WS.WebSocket -> Aff Unit
sendOffer conn socket = do
  offer <- createOffer conn
  offerStr <- createOfferString offer
  liftEffect $ WS.sendString socket offerStr
  setLocalDescription offer conn

receiveAnswerListener :: Effect EventListener
receiveAnswerListener = eventListener \e -> do
  pure unit

connect :: URL -> Aff RTCPeerConnection
connect (URL url) = do
  conn <- liftEffect $ newRTCPeerConnection defaultRTCConfiguration
  socket <- liftEffect $ WS.create url []
  liftEffect $ do
    el <- (eventListener \_ -> void $ launchAff $ do
      log "open"
      sendOffer conn socket)
    addEventListener
      WSET.onOpen
      el
      false
      (WS.toEventTarget socket)
  liftEffect $ do
    el <- receiveAnswerListener
    addEventListener
      WSET.onMessage
      el
      false
      (WS.toEventTarget socket)
  pure conn

sendAnswer ::RTCPeerConnection -> WS.WebSocket -> Aff Unit
sendAnswer conn socket = do
  answer <- createAnswer conn
  setLocalDescription answer conn
  answerStr <- createAnswerString answer
  liftEffect $ WS.sendString socket answerStr

receiveOfferListener :: Effect EventListener
receiveOfferListener = eventListener \e -> do
  pure unit

serve :: URL -> Aff RTCPeerConnection
serve (URL url) = do
  conn <- liftEffect $ newRTCPeerConnection defaultRTCConfiguration
  socket <- liftEffect $ WS.create url []
  liftEffect $ do
    el <- (eventListener \_ -> void $ launchAff $ do
      log "open")
    addEventListener
      WSET.onOpen
      el
      false
      (WS.toEventTarget socket)
  liftEffect $ do
    el <- receiveOfferListener
    addEventListener
      WSET.onMessage
      el
      false
      (WS.toEventTarget socket)
  pure conn
