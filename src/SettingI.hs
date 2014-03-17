{-# LANGUAGE TemplateHaskell #-}
module SettingI where

import Data.Aeson ()
import Data.Aeson.TH
import Data.HashMap.Lazy

import PluginI
import Config

data SettingI =
     SettingI { plugin         :: Maybe (HashMap String PluginI)
              , config         :: Maybe Config
              , filetypeScript :: Maybe (HashMap String String)
              , beforeScript   :: Maybe String
              , afterScript    :: Maybe String
     } deriving (Eq, Show)
$(deriveJSON defaultOptions ''SettingI)

