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
     Mapping { mapName    :: Text
             , mapRepText :: Text
             , mapUnique  :: MapUnique
             , mapSilent  :: MapSilent
             , mapMode    :: Mode
     } deriving Eq

instance Display Mapping where
  displayBuilder m =
    Builder.fromText $ unwords $ filter (not . null)
        [ display (mapMode m) <> "noremap"
        , display (mapUnique m)
       <> display (mapSilent m)
        , mapName m
        , mapRepText m
        ]

defaultMapping :: Mapping
defaultMapping
  = Mapping { mapName    = ""
            , mapRepText = ""
            , mapUnique  = MapUnique
            , mapSilent  = MapSilent
            , mapMode    = NormalMode
  }

