module Hangouts.Format (
  Config(..)
  , formatLine
  , formatJson
  ) where

import Hangouts.Format.Internal.Format (Config(..))
import Hangouts.Format.Line (formatLine)
import Hangouts.Format.Json (formatJson)
