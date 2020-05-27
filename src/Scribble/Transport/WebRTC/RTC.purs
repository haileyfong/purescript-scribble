module WebRTC.RTC
( RTCPeerConnection(..)
, hasRTC
, RTCSessionDescription(..)
, RTCIceServer(..)
, RTCConfiguration(..)
, IceEvent(..)
, RTCTrackEvent(..)
, RTCIceCandidate(..)
, RTCDataChannel(..)
, RTCSignalingState(..)
, newRTCPeerConnection
, defaultRTCConfiguration
, closeRTCPeerConnection
, addStream
, onicecandidate
, ontrack
, createOffer
, createAnswer
, setLocalDescription
, setRemoteDescription
, newRTCSessionDescription
, iceEventCandidate
, addIceCandidate
, createDataChannel
, send
, onmessage
, ondatachannel
, onopen
, onclose
, signalingState
) where

import Prelude
import Control.Alt ((<|>))

import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect (Effect)
import Data.Maybe (Maybe(..))
import WebRTC.MediaStream (MediaStream, MediaStreamTrack)

import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Core (jsonEmptyObject, jsonSingletonObject)
import Data.Argonaut.Decode.Combinators (getField)
import Data.NonEmpty (NonEmpty)
import Data.Argonaut.Encode.Combinators ((~>), (:=))

foreign import hasRTC :: Boolean

foreign import data RTCPeerConnection :: Type

data RTCIceServer
  = STUNServer { urls :: Array String }
  | TURNServer { urls :: Array String
               , username :: String
               , credential :: String
               }

data RTCSignalingState
  = Stable
  | HaveLocalOffer
  | HaveRemoteOffer
  | HaveLocalProvisionalAnswer
  | HaveRemoteProvisionalAnswer
  | UnknownValue String

derive instance eqRTCSignalingState :: Eq RTCSignalingState


type RTCConfiguration =
  { bundlePolicy :: Maybe String
  -- certificates :: (not yet implemented)
  , iceServers :: Array RTCIceServer
  , iceCandidatePoolSize :: Int
  , iceTransportPolicy :: String
  , peerIdentity :: Maybe String
  -- , rtcpMuxPolicy (at risk due to lack of implementor interest)
  }


defaultRTCConfiguration :: RTCConfiguration
defaultRTCConfiguration =
  { bundlePolicy: Nothing
  , iceServers: []
  , iceCandidatePoolSize: 0
  , iceTransportPolicy: "all"
  , peerIdentity: Nothing
  }


foreign import newRTCPeerConnection
  :: RTCConfiguration -> Effect RTCPeerConnection


foreign import closeRTCPeerConnection
  :: RTCPeerConnection -> Effect Unit


foreign import onclose
  :: Effect Unit -> RTCPeerConnection -> Effect Unit


foreign import addStream
  :: MediaStream -> RTCPeerConnection -> Effect Unit


-- Getting the signalling state
foreign import _signalingState :: RTCPeerConnection -> Effect String


stringToRTCSignalingState :: String -> RTCSignalingState
stringToRTCSignalingState =
  case _ of
    "stable"               -> Stable
    "have-local-offer"     -> HaveLocalOffer
    "have-remote-offer"    -> HaveRemoteOffer
    "have-local-pranswer"  -> HaveLocalProvisionalAnswer
    "have-remote-pranswer" -> HaveRemoteProvisionalAnswer
    other                  -> UnknownValue other


signalingState :: RTCPeerConnection -> Effect RTCSignalingState
signalingState pc = stringToRTCSignalingState <$> _signalingState pc


foreign import data IceEvent :: Type


type RTCIceCandidate = { sdpMLineIndex :: Int
                       , sdpMid :: String
                       , candidate :: String
                       }


foreign import _iceEventCandidate
  :: ∀ a. Maybe a ->
               (a -> Maybe a) ->
               IceEvent ->
               Maybe RTCIceCandidate


iceEventCandidate :: IceEvent -> Maybe RTCIceCandidate
iceEventCandidate = _iceEventCandidate Nothing Just


foreign import addIceCandidate
  :: RTCIceCandidate -> RTCPeerConnection -> Effect Unit


foreign import onicecandidate
  :: (IceEvent -> Effect Unit) -> RTCPeerConnection -> Effect Unit


type RTCTrackEvent = { streams :: Array MediaStream, track :: MediaStreamTrack }


foreign import ontrack
  :: (RTCTrackEvent -> Effect Unit) -> RTCPeerConnection -> Effect Unit


-- foreign import data RTCSessionDescription :: Type


foreign import newRTCSessionDescription
  :: { sdp :: String, "type" :: String } -> RTCSessionDescription


foreign import _createOffer
  :: RTCPeerConnection -> EffectFnAff RTCSessionDescription


createOffer :: RTCPeerConnection -> Aff RTCSessionDescription
createOffer = fromEffectFnAff <<< _createOffer


foreign import _createAnswer
  :: RTCPeerConnection -> EffectFnAff RTCSessionDescription


createAnswer :: RTCPeerConnection -> Aff RTCSessionDescription
createAnswer = fromEffectFnAff <<< _createAnswer


foreign import _setLocalDescription
  :: RTCSessionDescription -> RTCPeerConnection -> EffectFnAff Unit


setLocalDescription :: RTCSessionDescription -> RTCPeerConnection -> Aff Unit
setLocalDescription desc pc = fromEffectFnAff $ _setLocalDescription desc pc


foreign import _setRemoteDescription
  :: RTCSessionDescription -> RTCPeerConnection -> EffectFnAff Unit


setRemoteDescription :: RTCSessionDescription -> RTCPeerConnection -> Aff Unit
setRemoteDescription desc pc = fromEffectFnAff $ _setRemoteDescription desc pc


foreign import data RTCDataChannel :: Type

foreign import createDataChannel
  :: String -> RTCPeerConnection -> Effect RTCDataChannel

foreign import send
  :: String -> RTCDataChannel -> Effect Unit

foreign import onmessage
  :: (String -> Effect Unit) -> RTCDataChannel -> Effect Unit

foreign import ondatachannel
  :: (RTCDataChannel -> Effect Unit) -> RTCPeerConnection -> Effect Unit

foreign import onopen :: (Effect Unit) -> RTCDataChannel -> Effect Unit

data ServerType
  = STUN { urls :: NonEmpty Array String }
  | TURN { urls :: NonEmpty Array String, credentialType :: Maybe String, credential :: Maybe String, username :: Maybe String }

instance serverTypeEncodeJson :: EncodeJson ServerType where
  encodeJson (STUN s) = jsonSingletonObject "urls" (encodeJson s.urls)
  encodeJson (TURN t) = (
    "urls" := t.urls
    ~> "credentialType" := t.credentialType
    ~> "credential" := t.credential
    ~> "username" := t.username
    ~> jsonEmptyObject
  )

instance serverTypeDecodeJson :: DecodeJson ServerType where
  decodeJson json' = getTurn json' <|> getStun json'
    where
      getTurn json = do
        obj <- decodeJson json
        credentialType <- getField obj "credentialType"
        credential <- getField obj "credential"
        username <- getField obj "username"
        urls <- getField obj "urls"
        pure $ TURN { credentialType, credential, username, urls }
      getStun json = do
        obj <- decodeJson json
        urls <- getField obj "urls"
        pure $ STUN { urls }

newtype RTCSessionDescription = RTCSessionDescription { sdp :: String, "type" :: String }

rtcSessionDescriptionSdp :: RTCSessionDescription -> String
rtcSessionDescriptionSdp (RTCSessionDescription r) = r.sdp

rtcSessionDescriptionType :: RTCSessionDescription -> String
rtcSessionDescriptionType (RTCSessionDescription {"type" : t}) = t

instance rTCSessionDescriptionEncodeJSON :: EncodeJson RTCSessionDescription where
  encodeJson (RTCSessionDescription {"sdp" : sdp, "type" : t}) =
    ("sdp" := sdp
    ~> "type" := t
    ~> jsonEmptyObject)

instance rTCSessionDescriptionDecodeJSON :: DecodeJson RTCSessionDescription where
  decodeJson json = do
    obj <- decodeJson json
    sdp <- getField obj "sdp"
    t <- getField obj "type"
    pure $ RTCSessionDescription { "sdp" : sdp, "type" : t }