{-# LANGUAGE OverloadedStrings #-}

module Cirrus.Deploy (runDeploy) where

import Cirrus.Encode (encode)
import Control.Exception.Lens (catching_)
import Control.Lens ((&), (?~), (.~))
import Control.Monad (void)
import Data.ByteString.Lazy (toStrict)
import Data.Text.Encoding (decodeUtf8)
import Data.Text (Text)
import Network.AWS
import Network.AWS.CloudFormation
import System.IO (stdout)
import qualified Cirrus.Types as C (Stack(..))

create :: C.Stack -> CreateStack
create s = createStack (C.stackName s) & csTemplateBody ?~ templateBody s

update :: C.Stack -> UpdateStack
update s = updateStack (C.stackName s) & usTemplateBody ?~ templateBody s

templateBody :: C.Stack -> Text
templateBody = decodeUtf8 . toStrict . encode . C.stackTemplate

-- TODO return an Either?
deploy :: C.Stack -> AWS ()
deploy s = catching_ _AlreadyExistsException (run create) (run update)
  where run f = void . send $ f s

runDeploy :: C.Stack -> IO ()
runDeploy s = do
  env <- newEnv NorthVirginia Discover
  logger <- newLogger Debug stdout

  runResourceT . runAWS (env & envLogger .~ logger) $ deploy s
