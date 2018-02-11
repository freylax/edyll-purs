module Data.Path where

import Prelude
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic (gDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (gEncodeJson)
import Data.Generic (class Generic, gShow)

data Path
  = Directory String
  | File String

derive instance genericPath :: Generic Path

instance showPath :: Show Path where show = gShow
instance encodeJsonPath :: EncodeJson Path where encodeJson = gEncodeJson
instance decodeJsonPath :: DecodeJson Path where decodeJson = gDecodeJson

data DirListing
  = DirListing (Array String) (Array Path)

currentDir :: DirListing -> (Array String)
currentDir (DirListing c _) = c

derive instance genericDirListing :: Generic DirListing

instance showDirListing :: Show DirListing where show = gShow
                                                 
instance encodeJsonDirListing :: EncodeJson DirListing
  where encodeJson = gEncodeJson

instance decodeJsonDirListing :: DecodeJson DirListing
  where decodeJson = gDecodeJson


