{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hangouts.Format

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as B
import Data.Time (getCurrentTimeZone)
import Data.Text.Prettyprint.Doc.Render.Text (putDoc)

main :: IO ()
main = do
  json <- B.getContents
  tz <- getCurrentTimeZone
  let config = Config tz
  let convos = decode json
  case convos of
    Just convos' -> putDoc $ format config convos'
    Nothing -> putStrLn "error"
