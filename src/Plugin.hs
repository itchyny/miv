{-# LANGUAGE TemplateHaskell #-}
module Plugin where

import Data.List (isSuffixOf, isPrefixOf)
import Data.Aeson ()
import Data.Aeson.TH
import Data.Maybe (fromMaybe)
import Control.Monad (liftM2)

import qualified PluginI as PI

data Plugin =
     Plugin { name          :: String
            , filetypes     :: [String]
            , commands      :: [String]
            , functions     :: [String]
            , mappings      :: [String]
            , enable        :: String
            , sync          :: Bool
            , insert        :: Bool
            , mapleader     :: String
            , script        :: [String]
            , afterScript   :: [String]
            , beforeScript  :: [String]
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
           , filetypes    = fromMaybe [] (liftM2 (:) (PI.filetype p) (PI.filetypes p))
           , commands     = fromMaybe [] (liftM2 (:) (PI.command p) (PI.commands p))
           , functions    = fromMaybe [] (liftM2 (:) (PI.function p) (PI.functions p))
           , mappings     = fromMaybe [] (liftM2 (:) (PI.mapping p) (PI.mappings p))
           , enable       = fromMaybe "" (PI.enable p)
           , sync         = fromMaybe True (PI.sync p)
           , insert       = fromMaybe False (PI.insert p)
           , mapleader    = fromMaybe "" (PI.mapleader p)
           , script       = fromMaybe [] (PI.script p)
           , afterScript  = fromMaybe [] (PI.afterScript p)
           , beforeScript = fromMaybe [] (PI.beforeScript p)
  }

