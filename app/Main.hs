{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.List (intercalate, sortOn)
import qualified Data.ByteString.Lazy as B
import Data.Time.Format

printConvo :: Conversation -> IO ()
printConvo Conversation {
  conversationId = id
  , participants = p
  , events = e
  } = putStrLn $ id ++ ": " ++ formatParticipants p ++
  "\n=========\n" ++ (intercalate "\n" . map formatEvent . sortOn time ) e

formatEvent :: Event -> String
formatEvent Event {senderId = senderId, time = time, text = Just text} =
  "[" ++ formatTime defaultTimeLocale "%c" time ++ "] " ++ senderId ++ ": " ++ text
formatEvent Event {text = Nothing} = ""

formatParticipants :: [Participant] -> String
formatParticipants = intercalate ", " . map (fromMaybe "unknown" . name)

main :: IO ()
main = do
  json <- B.getContents
  let convos = conversations <$> decode json
  case convos of
    Just convos' -> mapM_ printConvo convos'
    Nothing -> putStrLn "error"
