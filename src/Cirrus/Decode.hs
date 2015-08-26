module Cirrus.Decode (decode) where

import Cirrus.Types (Config)
import Data.Yaml (decodeEither)
import qualified Data.ByteString as BS

decode :: BS.ByteString -> Either String Config
decode = decodeEither
