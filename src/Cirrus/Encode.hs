{-# LANGUAGE OverloadedStrings #-}

module Cirrus.Encode (encode) where

import Cirrus.Types (Template)
import qualified Data.Aeson.Encode.Pretty as AE (Config(..), encodePretty', keyOrder)
import qualified Data.ByteString.Lazy as LBS

encode :: Template -> LBS.ByteString
encode = AE.encodePretty' (AE.Config 2 ord)
  where ord = keyOrder `mappend` compare
        keyOrder = AE.keyOrder ["AWSTemplateFormatVersion", "Type", "Properties"]
