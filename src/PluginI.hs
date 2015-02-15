{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module PluginI where

import Data.Aeson ()
import Data.Aeson.TH
import Data.Text (Text)

data PluginI =
     PluginI { filetype      :: Maybe Text
             , filetypes     :: Maybe [Text]
             , command       :: Maybe Text
             , commands      :: Maybe [Text]
             , function      :: Maybe Text
             , functions     :: Maybe [Text]
             , mapping       :: Maybe Text
             , mappings      :: Maybe [Text]
             , mapmode       :: Maybe Text
             , mapmodes      :: Maybe [Text]
             , enable        :: Maybe Text
             , sync          :: Maybe Bool
             , insert        :: Maybe Bool
             , mapleader     :: Maybe Text
             , script        :: Maybe Text
             , afterScript   :: Maybe Text
             , beforeScript  :: Maybe Text
             , dependon      :: Maybe [Text]
             , dependedby    :: Maybe [Text]
             , loadafter     :: Maybe [Text]
             , loadbefore    :: Maybe [Text]
             , submodule     :: Maybe Bool
      } deriving (Eq, Show)
$(deriveJSON defaultOptions ''PluginI)

