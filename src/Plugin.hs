{-# LANGUAGE TemplateHaskell #-}
module Plugin where

import Data.List (isSuffixOf, isPrefixOf)
import Data.Aeson ()
import Data.Aeson.TH
import Control.Monad (liftM2)

import qualified PluginI as PI

data Plugin =
     Plugin { name          :: String
            , filetypes     :: Maybe [String]
            , commands      :: Maybe [String]
            , functions     :: Maybe [String]
            , mappings      :: Maybe [String]
            , enable        :: Maybe String
            , sync          :: Maybe Bool
            , insert        :: Maybe Bool
            , mapleader     :: Maybe String
            , script        :: Maybe [String]
            , afterScript   :: Maybe [String]
            , beforeScript  :: Maybe [String]
     } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Plugin)

rtpName :: Plugin -> String
rtpName plg = subst (name plg)
  where subst :: String -> String
        subst ('/':s) = subst s
        subst s | '/' `elem` s = subst (dropWhile (/='/') s)
                | ".vim" `isSuffixOf` s = take (length s - 4) s
                | "-vim" `isSuffixOf` s = take (length s - 4) s
                | "vim-" `isPrefixOf` s = drop 4 s
                | otherwise = filter (`notElem`"!?;:/<>()[]{}|~'\"") s

toPlugin :: String -> PI.PluginI -> Plugin
toPlugin n p
  = Plugin { name = n
           , filetypes = liftM2 (:) (PI.filetype p) (PI.filetypes p)
           , commands = liftM2 (:) (PI.command p) (PI.commands p)
           , functions = liftM2 (:) (PI.function p) (PI.functions p)
           , mappings = liftM2 (:) (PI.mapping p) (PI.mappings p)
           , enable = PI.enable p
           , sync = PI.sync p
           , insert = PI.insert p
           , mapleader = PI.mapleader p
           , script = PI.script p
           , afterScript = PI.afterScript p
           , beforeScript = PI.beforeScript p
  }

