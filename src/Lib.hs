{-# LANGUAGE OverloadedStrings #-}

module Lib
    (
        Participant(..)
      , Event(..)
      , Conversation(..)
      , Conversations(..)
    ) where

import Control.Monad (foldM, (>=>))
import Data.Aeson
import Data.Aeson.Types
import Data.Text
import qualified Data.ByteString.Lazy as B

import Data.Time.Clock.POSIX
import Data.Time

data Participant = Participant {
    name :: Maybe String
  , participantId :: String
  } deriving Show

data Event = Event {
   senderId :: String
   , time :: UTCTime
   , text :: Maybe String
   } deriving Show

data Conversation = Conversation {
    conversationId :: String
  , participants :: [Participant]
  , events :: [Event]
  } deriving Show

data Conversations = Conversations {
  conversations :: [Conversation]
  } deriving Show

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

convo :: B.ByteString
convo = "{\"conversation\":{\"conversation_id\":{\"id\":\"UgwI7hlb02Y6-eJtJ5t4AaABAQ\"},\"conversation\":{\"id\":{\"id\":\"UgwI7hlb02Y6-eJtJ5t4AaABAQ\"},\"type\":\"GROUP\",\"self_conversation_state\":{\"self_read_state\":{\"participant_id\":{\"gaia_id\":\"116287411739820504313\",\"chat_id\":\"116287411739820504313\"},\"latest_read_timestamp\":\"1451663113854218\"},\"status\":\"ACTIVE\",\"notification_level\":\"RING\",\"view\":[\"ARCHIVED_VIEW\"],\"inviter_id\":{\"gaia_id\":\"116287411739820504313\",\"chat_id\":\"116287411739820504313\"},\"invite_timestamp\":\"1451406096284000\",\"sort_timestamp\":\"1451663113854218\",\"active_timestamp\":\"1451406096284000\",\"delivery_medium_option\":[{\"delivery_medium\":{\"medium_type\":\"BABEL_MEDIUM\"},\"current_default\":true}],\"is_guest\":false},\"read_state\":[{\"participant_id\":{\"gaia_id\":\"116287411739820504313\",\"chat_id\":\"116287411739820504313\"},\"latest_read_timestamp\":\"1451663113854218\"},{\"participant_id\":{\"gaia_id\":\"107163393235893889056\",\"chat_id\":\"107163393235893889056\"},\"latest_read_timestamp\":\"0\"},{\"participant_id\":{\"gaia_id\":\"107272356564501317136\",\"chat_id\":\"107272356564501317136\"},\"latest_read_timestamp\":\"0\"}],\"has_active_hangout\":false,\"otr_status\":\"ON_THE_RECORD\",\"otr_toggle\":\"ENABLED\",\"current_participant\":[{\"gaia_id\":\"116287411739820504313\",\"chat_id\":\"116287411739820504313\"},{\"gaia_id\":\"107272356564501317136\",\"chat_id\":\"107272356564501317136\"},{\"gaia_id\":\"107163393235893889056\",\"chat_id\":\"107163393235893889056\"}],\"participant_data\":[{\"id\":{\"gaia_id\":\"116287411739820504313\",\"chat_id\":\"116287411739820504313\"},\"fallback_name\":\"Matus Goljer\",\"invitation_status\":\"ACCEPTED_INVITATION\",\"participant_type\":\"GAIA\",\"new_invitation_status\":\"ACCEPTED_INVITATION\",\"in_different_customer_as_requester\":false,\"domain_id\":\"105250506097979753968\"},{\"id\":{\"gaia_id\":\"107163393235893889056\",\"chat_id\":\"107163393235893889056\"},\"invitation_status\":\"ACCEPTED_INVITATION\",\"participant_type\":\"GAIA\",\"new_invitation_status\":\"ACCEPTED_INVITATION\",\"in_different_customer_as_requester\":false,\"domain_id\":\"105250506097979753968\"},{\"id\":{\"gaia_id\":\"107272356564501317136\",\"chat_id\":\"107272356564501317136\"},\"invitation_status\":\"ACCEPTED_INVITATION\",\"participant_type\":\"GAIA\",\"new_invitation_status\":\"ACCEPTED_INVITATION\",\"in_different_customer_as_requester\":false,\"domain_id\":\"105250506097979753968\"}],\"fork_on_external_invite\":false,\"network_type\":[\"BABEL\"],\"force_history_state\":\"NO_FORCE\",\"group_link_sharing_status\":\"LINK_SHARING_OFF\"}},\"events\":[{\"conversation_id\":{\"id\":\"UgwI7hlb02Y6-eJtJ5t4AaABAQ\"},\"sender_id\":{\"gaia_id\":\"116287411739820504313\",\"chat_id\":\"116287411739820504313\"},\"timestamp\":\"1451406097005341\",\"self_event_state\":{\"user_id\":{\"gaia_id\":\"116287411739820504313\",\"chat_id\":\"116287411739820504313\"},\"client_generated_id\":\"1451406096849\",\"notification_level\":\"RING\"},\"membership_change\":{\"type\":\"JOIN\",\"participant_id\":[{\"gaia_id\":\"107163393235893889056\",\"chat_id\":\"107163393235893889056\"}],\"leave_reason\":\"LEAVE_REASON_UNKNOWN\"},\"event_id\":\"88LyXzye88b88LyY3qxeB8\",\"advances_sort_timestamp\":true,\"event_otr\":\"ON_THE_RECORD\",\"delivery_medium\":{\"medium_type\":\"BABEL_MEDIUM\"},\"event_type\":\"ADD_USER\",\"event_version\":\"1451406097005341\"},{\"conversation_id\":{\"id\":\"UgwI7hlb02Y6-eJtJ5t4AaABAQ\"},\"sender_id\":{\"gaia_id\":\"116287411739820504313\",\"chat_id\":\"116287411739820504313\"},\"timestamp\":\"1451406097867125\",\"self_event_state\":{\"user_id\":{\"gaia_id\":\"116287411739820504313\",\"chat_id\":\"116287411739820504313\"},\"client_generated_id\":\"1451406097822\",\"notification_level\":\"RING\"},\"hangout_event\":{\"event_type\":\"START_HANGOUT\",\"participant_id\":[{\"gaia_id\":\"107272356564501317136\",\"chat_id\":\"107272356564501317136\"},{\"gaia_id\":\"116287411739820504313\",\"chat_id\":\"116287411739820504313\"}],\"media_type\":\"AUDIO_VIDEO\"},\"event_id\":\"88LyXzye88b88LyYA_l6S5\",\"advances_sort_timestamp\":true,\"event_otr\":\"ON_THE_RECORD\",\"delivery_medium\":{\"medium_type\":\"BABEL_MEDIUM\"},\"event_type\":\"HANGOUT_EVENT\",\"event_version\":\"1451406097867125\"},{\"conversation_id\":{\"id\":\"UgwI7hlb02Y6-eJtJ5t4AaABAQ\"},\"sender_id\":{\"gaia_id\":\"107272356564501317136\",\"chat_id\":\"107272356564501317136\"},\"timestamp\":\"1451406330858088\",\"self_event_state\":{\"user_id\":{\"gaia_id\":\"116287411739820504313\",\"chat_id\":\"116287411739820504313\"},\"notification_level\":\"RING\"},\"hangout_event\":{\"event_type\":\"END_HANGOUT\",\"participant_id\":[{\"gaia_id\":\"107272356564501317136\",\"chat_id\":\"107272356564501317136\"},{\"gaia_id\":\"107163393235893889056\",\"chat_id\":\"107163393235893889056\"},{\"gaia_id\":\"116287411739820504313\",\"chat_id\":\"116287411739820504313\"}],\"hangout_duration_secs\":\"428\",\"media_type\":\"AUDIO_VIDEO\"},\"event_id\":\"88LyXzye88b88Lyzbp9F2n\",\"advances_sort_timestamp\":true,\"event_otr\":\"ON_THE_RECORD\",\"delivery_medium\":{\"medium_type\":\"BABEL_MEDIUM\"},\"event_type\":\"HANGOUT_EVENT\",\"event_version\":\"1451406330858088\"},{\"conversation_id\":{\"id\":\"UgwI7hlb02Y6-eJtJ5t4AaABAQ\"},\"sender_id\":{\"gaia_id\":\"107272356564501317136\",\"chat_id\":\"107272356564501317136\"},\"timestamp\":\"1451663113854218\",\"self_event_state\":{\"user_id\":{\"gaia_id\":\"116287411739820504313\",\"chat_id\":\"116287411739820504313\"},\"notification_level\":\"RING\"},\"chat_message\":{\"message_content\":{\"segment\":[{\"type\":\"TEXT\",\"text\":\"Test\"}]}},\"event_id\":\"88LyXzye88b88TclBzD8UI\",\"advances_sort_timestamp\":true,\"event_otr\":\"ON_THE_RECORD\",\"delivery_medium\":{\"medium_type\":\"BABEL_MEDIUM\"},\"event_type\":\"REGULAR_CHAT_MESSAGE\",\"event_version\":\"1451663113854218\"}]}"
