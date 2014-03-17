{-# LANGUAGE TemplateHaskell #-}
module PluginI where

import Data.Aeson ()
import Data.Aeson.TH

data PluginI =
     PluginI { filetype      :: Maybe String
             , filetypes     :: Maybe [String]
             , command       :: Maybe String
             , commands      :: Maybe [String]
             , function      :: Maybe String
             , functions     :: Maybe [String]
             , mapping       :: Maybe String
             , mappings      :: Maybe [String]
             , enable        :: Maybe String
             , sync          :: Maybe Bool
             , insert        :: Maybe Bool
             , mapleader     :: Maybe String
             , script        :: Maybe String
             , afterScript   :: Maybe String
             , beforeScript  :: Maybe String
             , dependon      :: Maybe [String]
             , dependedby    :: Maybe [String]
             , loadafter     :: Maybe [String]
             , loadbefore    :: Maybe [String]
      } deriving (Eq, Show)
$(deriveJSON defaultOptions ''PluginI)

