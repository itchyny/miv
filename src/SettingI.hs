{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module SettingI where

import Data.Aeson ()
import Data.Aeson.TH
import Data.HashMap.Lazy
import qualified Data.Text as T

import PluginI

data SettingI =
     SettingI { plugin         :: Maybe (HashMap T.Text PluginI)
              , filetypeScript :: Maybe (HashMap T.Text T.Text)
              , beforeScript   :: Maybe T.Text
              , afterScript    :: Maybe T.Text
              , filetypeDetect :: Maybe (HashMap T.Text T.Text)
     } deriving (Eq, Show)
$(deriveJSON defaultOptions ''SettingI)

