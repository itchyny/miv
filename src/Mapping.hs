module Mapping where

import Mode

data MapUnique = MapUnique | MapNoUnique
               deriving Eq

instance Show MapUnique where
  show MapUnique = "<unique>"
  show MapNoUnique = ""

data MapSilent = MapSilent | MapNoSilent
               deriving Eq

instance Show MapSilent where
  show MapSilent = "<silent>"
  show MapNoSilent = ""

data Mapping =
     Mapping { mapName    :: String
             , mapRepText :: String
             , mapUnique  :: MapUnique
             , mapSilent  :: MapSilent
             , mapMode    :: Mode
     } deriving Eq

instance Show Mapping where
  show m = unwords (filter (/="")
          [ show (mapMode m) ++ "noremap"
          , show (mapUnique m)
         ++ show (mapSilent m)
          , mapName m
          , mapRepText m
          ])

defaultMapping :: Mapping
defaultMapping
  = Mapping { mapName    = ""
            , mapRepText = ""
            , mapUnique  = MapUnique
            , mapSilent  = MapSilent
            , mapMode    = NormalMode
  }

