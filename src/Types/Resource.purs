module Types.Resource where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), decodeJson, encodeJson)
import Data.Argonaut.Core (stringify)
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.UUID as UD

type Resource = 
  { resourceId :: UUID,
    resourceTitle :: String,
    resourceType :: ResourceType,
    isSegmented :: Boolean
  }

newtype UUID = UUID UD.UUID

uuidToString :: UUID -> String
uuidToString (UUID ud) = UD.toString ud

uuidFromString :: String -> Maybe UUID
uuidFromString s = uuid
  where
    innerUUID = UD.parseUUID s
    uuid = case innerUUID of
      Just ud -> Just $ UUID ud
      Nothing -> Nothing

derive instance newtypeUUID :: Newtype UUID _
derive newtype instance showUUID :: Show UUID

instance encodeJsonUUID :: EncodeJson UUID where
  encodeJson uuid = encodeJson (uuidToString uuid)

instance decodeJsonUUID :: DecodeJson UUID where
  decodeJson json = res
    where
      string = decodeJson json :: Either JsonDecodeError String
      res = case string of
        Left err -> Left err
        Right str -> note (TypeMismatch "UUID") (uuidFromString str)

data ResourceType = Audio | Video

instance showResourceType :: Show ResourceType where
  show Audio = "audio"
  show Video = "video"

resourceTypeToString :: ResourceType -> String
resourceTypeToString = case _ of
  Audio -> "audio"
  Video -> "video"

instance encodeJsonResourceType :: EncodeJson ResourceType where
  encodeJson resType = encodeJson (resourceTypeToString resType)

resourceTypeFromString :: String -> Maybe ResourceType
resourceTypeFromString = case _ of
  "audio" -> Just Audio
  "video" -> Just Video
  _ -> Nothing

instance decodeJsonResourceType :: DecodeJson ResourceType where
  decodeJson json = res
    where
      string = decodeJson json
      res = case string of
          Left err -> Left err
          Right str -> note (TypeMismatch "ResourceType") (resourceTypeFromString str)

printResourceDecodeResult :: Either JsonDecodeError Json -> String
printResourceDecodeResult res = case res of
  Left err -> show err
  Right el -> stringify el
