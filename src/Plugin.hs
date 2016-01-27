{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Plugin where

import Control.Applicative ((<|>))
import Data.Aeson
import qualified Data.Text as T
import Data.Text (Text, unpack)

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
            , after         :: [Text]
            , before        :: [Text]
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
                | otherwise = T.filter (`notElem`("!?;:/<>()[]{}|~'\"" :: String)) s

instance FromJSON Plugin where
  parseJSON = withObject "plugin" $ \o -> do
    let name = ""
    filetypes <- o .: "filetypes" <|> (fmap return <$> o .:? "filetype") .!= []
    commands <- o .: "commands" <|> (fmap return <$> o .:? "command") .!= []
    functions <- o .: "functions" <|> (fmap return <$> o .:? "function") .!= []
    mappings <- o .: "mappings" <|> (fmap return <$> o .:? "mapping") .!= []
    mapmodes <- o .: "mapmodes" <|> (fmap return <$> o .:? "mapmode") .!= []
    mapleader <- o .:? "mapleader" .!= ""
    insert <- o .:? "insert" .!= False
    enable <- o .:? "enable" .!= ""
    sync <- o .:? "sync" .!= True
    script <- T.lines <$> (o .:? "script") .!= ""
    after <- T.lines <$> (o .:? "after") .!= ""
    before <- T.lines <$> (o .:? "before") .!= ""
    dependon <- o .:? "dependon" .!= []
    dependedby <- o .:? "dependedby" .!= []
    loadafter <- o .:? "loadafter" .!= []
    loadbefore <- o .:? "loadbefore" .!= []
    submodule <- o .:? "submodule" .!= False
    return Plugin {..}

instance ToJSON Plugin where
  toJSON = const (object [])

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
         , after         = []
         , before        = []
         , dependon      = []
         , dependedby    = []
         , loadafter     = []
         , loadbefore    = []
         , submodule     = False
  }
