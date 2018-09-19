{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hangouts.Format
import Hangouts.Parser

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as B
import Data.Time (getCurrentTimeZone)
import Data.Text.Lazy.IO as TIO (putStrLn)
import Data.Text.Lazy (Text)

import Options.Applicative
import Data.Semigroup ((<>))

data OutputFormat
  = Line
  | Json

jsonOutput :: Parser OutputFormat
jsonOutput = flag' Line
  (  long "line"
  <> help "Output as tab-separated line data" )

lineOutput :: Parser OutputFormat
lineOutput = flag' Json
  (  long "json"
  <> help "Output as JSON" )

outputFormat :: Parser OutputFormat
outputFormat = jsonOutput <|> lineOutput

formatFn :: OutputFormat -> Config -> Conversations -> Text
formatFn Line = formatLine
formatFn Json = formatJson

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (outputFormat <**> helper)
      ( fullDesc
     <> progDesc "Read and output the Google Hangouts data in more convenient formats for further processing"
     <> header "hangouts-reader - process Google Hangouts Takeout data" )

run :: OutputFormat -> IO ()
run outputFormat = do
  json <- B.getContents
  tz <- getCurrentTimeZone
  let config = Config tz
  let convos = decode json
  case convos of
    Just convos' -> TIO.putStrLn $ formatFn outputFormat config convos'
    Nothing -> Prelude.putStrLn "error"
