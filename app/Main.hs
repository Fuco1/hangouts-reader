{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hangouts.Parser
import Hangouts.Format

import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.List (intercalate, sortOn, find)
import qualified Data.ByteString.Lazy as B
import Data.Time.Format
import Data.Time
import Data.Text.Prettyprint.Doc.Render.Text (putDoc)

main :: IO ()
main = do
  json <- B.getContents
  tz <- getCurrentTimeZone
  let config = Config tz
  let convos = decode json
  case convos of
    Just convos' -> do
      putDoc $ format config convos'
    Nothing -> putStrLn "error"
