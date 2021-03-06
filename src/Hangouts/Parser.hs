{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Hangouts.Parser
    (
        Participant(..)
      , Event(..)
      , Conversation(..)
      , Conversations(..)
      , getParticipantName
    ) where

import GHC.Generics

import Control.Monad ((>=>))
import Data.Aeson.Types
import Data.Text hiding (find)
import Data.Maybe (fromMaybe)
import Data.List (find)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time (UTCTime)

data Participant = Participant {
    name :: Maybe String
  , participantId :: String
  } deriving (Show, Generic)

data Event = Event {
   senderId :: String
   , time :: UTCTime
   , text :: Maybe String
   } deriving (Show, Generic)

data Conversation = Conversation {
    conversationId :: String
  , participants :: [Participant]
  , events :: [Event]
  } deriving (Show, Generic)

data Conversations = Conversations {
  conversations :: [Conversation]
  } deriving (Show, Generic)

instance ToJSON Participant
instance ToJSON Event
instance ToJSON Conversation
instance ToJSON Conversations

getParticipant :: String -> [Participant] -> Maybe Participant
getParticipant chatId = find (\x -> participantId x == chatId)

getParticipantName :: String -> [Participant] -> String
getParticipantName chatId = fromMaybe "unknown" . (\p -> getParticipant chatId p >>= name)

(@@) :: (FromJSON a) => Text -> Object -> Parser a
path @@ obj = obj .: path

instance FromJSON Participant where
  parseJSON (Object v) = do
    name <- v .:? "fallback_name"
    id <- v .: "id"
    chat_id <- id .: "chat_id"
    return $ Participant name chat_id

instance FromJSON Event where
  parseJSON (Object v) = do
    senderId<- (@@) "sender_id" >=> (@@) "chat_id" $ v
    time <- posixSecondsToUTCTime . fromIntegral . (read :: String -> Int) . Prelude.take 10 <$> v .: "timestamp"
    chatMessage <- v .:? "chat_message"
    text <- case chatMessage of
      Just msg -> do
        content <- msg .: "message_content"
        segments <- (content .:? "segment" .!= []) :: Parser [Object]
        message <- Prelude.concat <$>
          mapM (\x -> x .:? "text" .!= "\n") segments
        return $ Just message
      Nothing -> return Nothing
    return $ Event senderId time text

instance FromJSON Conversation where
  parseJSON (Object v) = do
    conv <- v .: "conversation"
    events <- v .: "events"
    participants <- (@@) "conversation" >=> (@@) "participant_data" $ conv
    id <- (@@) "conversation_id" >=> (@@) "id" $ conv
    return $ Conversation id participants events

instance FromJSON Conversations where
  parseJSON (Object v) = do
    convos <- v .: "conversations"
    return $ Conversations convos
