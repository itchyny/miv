{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module PluginI where

import Data.Aeson ()
import Data.Aeson.TH
import qualified Data.Text as T

data PluginI =
     PluginI { filetype      :: Maybe T.Text
             , filetypes     :: Maybe [T.Text]
             , command       :: Maybe T.Text
             , commands      :: Maybe [T.Text]
             , function      :: Maybe T.Text
             , functions     :: Maybe [T.Text]
             , mapping       :: Maybe T.Text
             , mappings      :: Maybe [T.Text]
             , mapmode       :: Maybe T.Text
             , mapmodes      :: Maybe [T.Text]
             , enable        :: Maybe T.Text
             , sync          :: Maybe Bool
             , insert        :: Maybe Bool
             , mapleader     :: Maybe T.Text
             , script        :: Maybe T.Text
             , afterScript   :: Maybe T.Text
             , beforeScript  :: Maybe T.Text
             , dependon      :: Maybe [T.Text]
             , dependedby    :: Maybe [T.Text]
             , loadafter     :: Maybe [T.Text]
             , loadbefore    :: Maybe [T.Text]
             , submodule     :: Maybe Bool
      } deriving (Eq, Show)
$(deriveJSON defaultOptions ''PluginI)

