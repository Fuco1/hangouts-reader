{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.List (intercalate, sortOn)
import qualified Data.ByteString.Lazy as B
import Data.Time.Format

printConversationParticipants :: [Conversation] -> IO ()
printConversationParticipants = mapM_
  (\Conversation {conversationId = id, participants = p} ->
      putStrLn $ id ++ ": " ++ formatParticipants p)

printConvo :: Conversation -> IO ()
printConvo Conversation {
  conversationId = id
  , participants = p
  , events = e
  } = putStrLn $ (intercalate "\n" . map (formatEvent id) . sortOn time ) e

formatEvent :: String -> Event -> String
formatEvent convId Event {senderId = senderId, time = time, text = Just text} =
  convId ++ " [" ++ formatTime defaultTimeLocale "%c" time ++ "] " ++ senderId ++ ": " ++ text
formatEvent _ Event {text = Nothing} = ""

formatParticipants :: [Participant] -> String
formatParticipants = intercalate ", " . map (fromMaybe "unknown" . name)

main :: IO ()
main = do
  json <- B.getContents
  let convos = conversations <$> decode json
  case convos of
    Just convos' -> do
      printConversationParticipants convos'
      mapM_ printConvo convos'
    Nothing -> putStrLn "error"
