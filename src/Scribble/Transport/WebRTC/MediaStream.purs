module WebRTC.MediaStream
( MediaStream(..)
, MediaStreamTrack(..)
, MediaStreamConstraints(..)
, hasUserMedia
, getUserMedia
, stopMediaStream
, playAudioStream
, mediaStreamToBlob
, createObjectURL
) where


import Prelude
import Unsafe.Coerce (unsafeCoerce)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect (Effect)
import Web.File.Blob (Blob)


foreign import data MediaStream :: Type
foreign import data MediaStreamTrack :: Type

foreign import createObjectURL :: Blob -> Effect String

foreign import hasUserMedia :: Boolean
foreign import _getUserMedia :: MediaStreamConstraints -> EffectFnAff MediaStream
foreign import stopMediaStream :: MediaStream -> Effect Unit
foreign import playAudioStream :: MediaStream -> Effect Unit


newtype MediaStreamConstraints =
    MediaStreamConstraints { video :: Boolean
                           , audio :: Boolean
                           }


getUserMedia :: MediaStreamConstraints -> Aff MediaStream
getUserMedia = fromEffectFnAff <<< _getUserMedia


mediaStreamToBlob :: MediaStream -> Blob
mediaStreamToBlob = unsafeCoerce
