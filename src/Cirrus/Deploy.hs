{-# LANGUAGE OverloadedStrings #-}

module Cirrus.Deploy (runDeploy) where

import Cirrus.Encode (encode)
import Cirrus.Types (Template)
import Control.Exception.Lens (catching_)
import Control.Lens ((&), (?~), (.~))
import Control.Monad (void)
import Data.ByteString.Lazy (toStrict)
import Data.Text.Encoding (decodeUtf8)
import Data.Text (Text)
import Network.AWS
import Network.AWS.CloudFormation
import System.IO (stdout)

type StackName = Text

create :: StackName -> Template -> CreateStack
create n t = createStack n & csTemplateBody ?~ templateBody t

update :: StackName -> Template -> UpdateStack
update n t = updateStack n & usTemplateBody ?~ templateBody t

templateBody :: Template -> Text
templateBody = decodeUtf8 . toStrict . encode

-- TODO return an Either?
deploy :: StackName -> Template -> AWS ()
deploy n t = catching_ _AlreadyExistsException (run create) (run update)
  where run f = void . send $ f n t

runDeploy :: StackName -> Template -> IO ()
runDeploy n t = do
  env <- newEnv NorthVirginia Discover
  logger <- newLogger Debug stdout

  runResourceT . runAWS (env & envLogger .~ logger) $ deploy n t
