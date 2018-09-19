{-# LANGUAGE OverloadedStrings #-}

module Hangouts.Format.Json (
  formatJson
  ) where

import Hangouts.Format.Internal.Format (Config)
import Hangouts.Parser (Conversations)
import Data.Aeson.Text (encodeToLazyText)
import Data.Text.Lazy (Text)

formatJson :: Config -> Conversations -> Text
formatJson _ = encodeToLazyText
