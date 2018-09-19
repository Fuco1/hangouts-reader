{-# LANGUAGE OverloadedStrings #-}

module Hangouts.Format.Internal.Format (
  Config(..)
  , formatTimestamp
  , (<++>)
  , join
  ) where

import Data.Time
import Data.Text.Prettyprint.Doc

data Config = Config {
  timeZone :: TimeZone
  }

tab :: Doc a
tab = "\t"

(<++>) :: Doc a -> Doc a -> Doc a
a <++> b = a <> tab <> b

join :: Foldable t => Doc a -> t (Doc a) -> Doc a
join separator = concatWith (surround separator)

formatTimestamp :: Config -> UTCTime -> Doc a
formatTimestamp Config {timeZone = tz} time =
  let local = utcToLocalTime tz time in
    pretty $ formatTime defaultTimeLocale "%F %T" local
