{-# LANGUAGE OverloadedStrings #-}

module Hangouts.Format.Line (
  formatLine
  ) where

import Hangouts.Format.Internal.Format
import Hangouts.Parser
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (renderLazy)
import Data.Text.Lazy (Text)
import Data.Maybe (fromMaybe)
import Data.ByteString.Lazy as B hiding (map)
import Data.List (sortOn)

formatLine :: Config -> Conversations -> Text
formatLine config convos = renderLazy . layoutPretty defaultLayoutOptions $
  header convos
  <> hardline
  <> hardline
  <> vsep (map (formatConvo config) . conversations $ convos)

header :: Conversations -> Doc a
header = vsep . map (\Conversation {conversationId = id, participants = p} ->
  pretty id <++> formatParticipants p) . conversations

formatParticipants :: [Participant] -> Doc a
formatParticipants = join ", " . map (pretty . fromMaybe "unknown" . name)

formatConvo :: Config -> Conversation -> Doc a
formatConvo config c@Conversation {
  conversationId = convId
  , participants = p
  , events = e
  } = vsep . map (formatEvent config c) . sortOn time $ e

formatEvent :: Config -> Conversation -> Event -> Doc a
formatEvent config Conversation {
  conversationId = convId
  , participants = p
  } Event {senderId = senderId, time = time, text = Just text} =
  pretty convId <++> formatTimestamp config time <++> (pretty $ getParticipantName senderId p) <++> pretty text
formatEvent _ _ Event {text = Nothing} = ""
