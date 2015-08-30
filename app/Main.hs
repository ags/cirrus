{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cirrus

import Control.Monad (join)
import Data.Text (pack)
import Options.Applicative

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

main :: IO ()
main = join $ execParser (info commands idm)

commands :: Parser (IO ())
commands = subparser
  ( command "parse" (info (parseCmd <$> argument str idm) idm)
 <> command "deploy" (info (deployCmd <$> argument str idm <*> argument str idm) idm)
  )

parseCmd :: FilePath -> IO ()
parseCmd f = do
  yml <- BS.readFile f
  either (fail . show) (LBS.putStr . encode) (decodeTemplate yml)

deployCmd :: String -> FilePath -> IO ()
deployCmd n f = do
  yml <- BS.readFile f
  either (fail . show) (runDeploy (pack n)) (decodeTemplate yml)

decodeTemplate :: BS.ByteString -> Either String Template
decodeTemplate yml = fmap fromConfig (decode yml)
