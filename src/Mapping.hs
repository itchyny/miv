{-# LANGUAGE OverloadedStrings #-}
module Mapping where

import qualified Data.Text as T

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
     Mapping { mapName    :: T.Text
             , mapRepText :: T.Text
             , mapUnique  :: MapUnique
             , mapSilent  :: MapSilent
             , mapMode    :: Mode
     } deriving Eq

instance Show Mapping where
  show m = unwords (filter (/="")
          [ show (mapMode m) ++ "noremap"
          , show (mapUnique m)
         ++ show (mapSilent m)
          , T.unpack (mapName m)
          , T.unpack (mapRepText m)
          ])

defaultMapping :: Mapping
defaultMapping
  = Mapping { mapName    = ""
            , mapRepText = ""
            , mapUnique  = MapUnique
            , mapSilent  = MapSilent
            , mapMode    = NormalMode
  }

