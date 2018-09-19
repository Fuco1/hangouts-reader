module Hangouts.Format.Internal.Format (
  Config(..)
  , formatTimestamp
  ) where

import Data.Time
import Data.Text.Prettyprint.Doc

data Config = Config {
  timeZone :: TimeZone
  }

formatTimestamp :: Config -> UTCTime -> Doc a
formatTimestamp Config {timeZone = tz} time =
  let local = utcToLocalTime tz time in
    pretty $ formatTime defaultTimeLocale "%F %T" local
