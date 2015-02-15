{-# LANGUAGE OverloadedStrings #-}
module Plugin where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text (Text, unpack)

import qualified PluginI as PI
import ShowText

data Plugin =
     Plugin { name          :: Text
            , filetypes     :: [Text]
            , commands      :: [Text]
            , functions     :: [Text]
            , mappings      :: [Text]
            , mapmodes      :: [Text]
            , insert        :: Bool
            , enable        :: Text
            , sync          :: Bool
            , mapleader     :: Text
            , script        :: [Text]
            , afterScript   :: [Text]
            , beforeScript  :: [Text]
            , dependon      :: [Text]
            , dependedby    :: [Text]
            , loadafter     :: [Text]
            , loadbefore    :: [Text]
            , submodule     :: Bool
     } deriving (Eq, Ord)

instance ShowText Plugin where
  show = rtpName

instance Show Plugin where
  show = unpack . rtpName

rtpName :: Plugin -> Text
rtpName plg = subst (name plg)
  where subst s | T.any (=='/') s = subst (T.tail $ T.dropWhile (/='/') s)
                | ".vim" `T.isSuffixOf` s = T.take (T.length s - 4) s
                | "-vim" `T.isSuffixOf` s = T.take (T.length s - 4) s
                | "vim-" `T.isPrefixOf` s = T.drop 4 s
                | otherwise = T.filter (`notElem`"!?;:/<>()[]{}|~'\"") s

toPlugin :: Text -> PI.PluginI -> Plugin
toPlugin n p
  = Plugin { name = n
           , filetypes    = maybe id (:) (PI.filetype p) (fromMaybe [] (PI.filetypes p))
           , commands     = maybe id (:) (PI.command p) (fromMaybe [] (PI.commands p))
           , functions    = maybe id (:) (PI.function p) (fromMaybe [] (PI.functions p))
           , mappings     = maybe id (:) (PI.mapping p) (fromMaybe [] (PI.mappings p))
           , mapmodes     = maybe id (:) (PI.mapmode p) (fromMaybe [] (PI.mapmodes p))
           , enable       = fromMaybe "" (PI.enable p)
           , sync         = fromMaybe True (PI.sync p)
           , insert       = fromMaybe False (PI.insert p)
           , mapleader    = fromMaybe "" (PI.mapleader p)
           , script       = T.lines $ fromMaybe "" (PI.script p)
           , afterScript  = T.lines $ fromMaybe "" (PI.afterScript p)
           , beforeScript = T.lines $ fromMaybe "" (PI.beforeScript p)
           , dependon     = fromMaybe [] (PI.dependon p)
           , dependedby   = fromMaybe [] (PI.dependedby p)
           , loadafter    = fromMaybe [] (PI.loadafter p)
           , loadbefore   = fromMaybe [] (PI.loadbefore p)
           , submodule    = fromMaybe False (PI.submodule p)
  }

defaultPlugin :: Plugin
defaultPlugin =
  Plugin { name          = ""
         , filetypes     = []
         , commands      = []
         , functions     = []
         , mappings      = []
         , mapmodes      = []
         , insert        = False
         , enable        = ""
         , sync          = False
         , mapleader     = ""
         , script        = []
         , afterScript   = []
         , beforeScript  = []
         , dependon      = []
         , dependedby    = []
         , loadafter     = []
         , loadbefore    = []
         , submodule     = False
  }
