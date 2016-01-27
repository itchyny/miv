{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module SettingI where

import Data.Aeson ()
import Data.Aeson.TH
import Data.HashMap.Lazy
import Data.Text (Text)

import Plugin

data SettingI =
     SettingI { plugin         :: Maybe (HashMap Text Plugin)
              , filetypeScript :: Maybe (HashMap Text Text)
              , beforeScript   :: Maybe Text
              , afterScript    :: Maybe Text
              , filetypeDetect :: Maybe (HashMap Text Text)
     } deriving (Eq, Show)
$(deriveJSON defaultOptions ''SettingI)

