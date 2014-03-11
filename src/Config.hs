{-# LANGUAGE TemplateHaskell #-}
module Config where

import Data.Aeson ()
import Data.Aeson.TH

data Config =
     Config { directory   :: Maybe String
     } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Config)

