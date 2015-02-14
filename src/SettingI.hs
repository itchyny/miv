{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module SettingI where

import Data.Aeson ()
import Data.Aeson.TH
import Data.HashMap.Lazy
import qualified Data.Text as T

import PluginI
import Config

data SettingI =
     SettingI { plugin         :: Maybe (HashMap String PluginI)
              , config         :: Maybe Config
              , filetypeScript :: Maybe (HashMap T.Text T.Text)
              , beforeScript   :: Maybe T.Text
              , afterScript    :: Maybe T.Text
              , filetypeDetect :: Maybe (HashMap T.Text T.Text)
     } deriving (Eq, Show)
$(deriveJSON defaultOptions ''SettingI)

