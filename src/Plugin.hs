{-# LANGUAGE TemplateHaskell #-}
module Plugin where

import Data.List (isSuffixOf, isPrefixOf)
import Data.Aeson ()
import Data.Aeson.TH
import Data.Maybe (fromMaybe)

import qualified PluginI as PI

data Plugin =
     Plugin { name          :: String
            , filetypes     :: [String]
            , commands      :: [String]
            , functions     :: [String]
            , mappings      :: [String]
            , insert        :: Bool
            , enable        :: String
            , sync          :: Bool
            , mapleader     :: String
            , script        :: [String]
            , afterScript   :: [String]
            , beforeScript  :: [String]
            , dependon      :: [String]
            , dependedby    :: [String]
            , loadafter     :: [String]
            , loadbefore    :: [String]
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
           , filetypes    = maybe id (:) (PI.filetype p) (fromMaybe [] (PI.filetypes p))
           , commands     = maybe id (:) (PI.command p) (fromMaybe [] (PI.commands p))
           , functions    = maybe id (:) (PI.function p) (fromMaybe [] (PI.functions p))
           , mappings     = maybe id (:) (PI.mapping p) (fromMaybe [] (PI.mappings p))
           , enable       = fromMaybe "" (PI.enable p)
           , sync         = fromMaybe True (PI.sync p)
           , insert       = fromMaybe False (PI.insert p)
           , mapleader    = fromMaybe "" (PI.mapleader p)
           , script       = fromMaybe [] (PI.script p)
           , afterScript  = fromMaybe [] (PI.afterScript p)
           , beforeScript = fromMaybe [] (PI.beforeScript p)
           , dependon     = fromMaybe [] (PI.dependon p)
           , dependedby   = fromMaybe [] (PI.dependedby p)
           , loadafter    = fromMaybe [] (PI.loadafter p)
           , loadbefore   = fromMaybe [] (PI.loadbefore p)
  }

defaultPlugin :: Plugin
defaultPlugin =
  Plugin { name          = []
         , filetypes     = []
         , commands      = []
         , functions     = []
         , mappings      = []
         , insert        = False
         , enable        = []
         , sync          = False
         , mapleader     = []
         , script        = []
         , afterScript   = []
         , beforeScript  = []
         , dependon      = []
         , dependedby    = []
         , loadafter     = []
         , loadbefore    = []
  }
