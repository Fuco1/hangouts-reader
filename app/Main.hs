{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.List (intercalate, sortOn, find)
import qualified Data.ByteString.Lazy as B
import Data.Time.Format
import Data.Time

printConversationParticipants :: [Conversation] -> IO ()
printConversationParticipants = mapM_
  (\Conversation {conversationId = id, participants = p} ->
      putStrLn $ id ++ ": " ++ formatParticipants p)

printConvo :: Conversation -> IO ()
printConvo c@Conversation {
  conversationId = id
  , participants = p
  , events = e
  } = do
  tz <- getCurrentTimeZone
  putStrLn $ (intercalate "\n" . map (formatEvent tz c) . sortOn time) e

formatEvent :: TimeZone -> Conversation -> Event -> String
formatEvent tz Conversation {
  conversationId = convId
  , participants = p
  } Event {senderId = senderId, time = time, text = Just text} =
  convId ++ " [" ++ formatTimestamp tz time ++ "] " ++ (getParticipantName senderId p) ++ ": " ++ text
formatEvent _ _ Event {text = Nothing} = ""

formatTimestamp :: TimeZone -> UTCTime -> String
formatTimestamp tz time =
  let local = utcToLocalTime tz time in
    formatTime defaultTimeLocale "%c" local

getParticipant :: String -> [Participant] -> Maybe Participant
getParticipant chatId = find (\x -> participantId x == chatId)

getParticipantName :: String -> [Participant] -> String
getParticipantName chatId = fromMaybe "unknown" . (\p -> getParticipant chatId p >>= name)

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
