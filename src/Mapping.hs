module Mapping where

import Data.Text (Text, unwords, null)
import Data.Text.Builder.Linear qualified as Builder
import Data.Text.Display (Display(..), display)
import Prelude hiding (unwords, null)

import Mode

data MapUnique = MapUnique | MapNoUnique
               deriving Eq

instance Display MapUnique where
  displayBuilder = Builder.fromText . \case
    MapUnique   -> "<unique>"
    MapNoUnique -> ""

data MapSilent = MapSilent | MapNoSilent
               deriving Eq

instance Display MapSilent where
  displayBuilder = Builder.fromText . \case
    MapSilent   -> "<silent>"
    MapNoSilent -> ""

data Mapping =
  Mapping {
    name    :: Text,
    repText :: Text,
    unique  :: MapUnique,
    silent  :: MapSilent,
    mode    :: Mode
  } deriving Eq

instance Display Mapping where
  displayBuilder m =
    Builder.fromText $ unwords $ filter (not . null)
        [ display m.mode <> "noremap",
          display m.unique <> display m.silent,
          m.name,
          m.repText
        ]

defaultMapping :: Mapping
defaultMapping =
  Mapping {
    name    = "",
    repText = "",
    unique  = MapUnique,
    silent  = MapSilent,
    mode    = NormalMode
  }

