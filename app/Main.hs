{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cirrus

import Control.Monad (join)
import Options.Applicative

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

main :: IO ()
main = join $ execParser (info commands idm)

commands :: Parser (IO ())
commands = subparser (command "parse" parseCmd)
  where parseCmd = info (parseConfig <$> argument str idm) idm

parseConfig :: String -> IO ()
parseConfig f = do
  yml <- BS.readFile f

  case decode yml of
    Right stack -> printTemplate $ fromConfig stack
    Left err    -> fail $ show err

printTemplate :: Template -> IO ()
printTemplate = LBS.putStr . encode
