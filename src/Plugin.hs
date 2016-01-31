{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Plugin where

import Control.Applicative ((<|>))
import Data.Aeson
import qualified Data.Text as T
import Data.Text (Text)

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

rtpName :: Plugin -> Text
rtpName plg = subst (name plg)
  where subst s | T.any (=='/') s = subst (T.tail $ T.dropWhile (/='/') s)
                | ".vim" `T.isSuffixOf` s = T.take (T.length s - 4) s
                | "-vim" `T.isSuffixOf` s = T.take (T.length s - 4) s
                | "vim-" `T.isPrefixOf` s = T.drop 4 s
                | otherwise = T.filter (`notElem`("!?;:/<>()[]{}|~'\"" :: String)) s

instance FromJSON Plugin where
  parseJSON = withObject "plugin" $ \o -> do
    name <- o .:? "name" .!= ""
    filetypes <- o .: "filetype" <|> (fmap return <$> o .:? "filetype") .!= []
    commands <- o .: "command" <|> (fmap return <$> o .:? "command") .!= []
    functions <- o .: "function" <|> (fmap return <$> o .:? "function") .!= []
    mappings <- o .: "mapping" <|> (fmap return <$> o .:? "mapping") .!= []
    mapmodes <- o .: "mapmode" <|> (fmap return <$> o .:? "mapmode") .!= []
    mapleader <- o .:? "mapleader" .!= ""
    insert <- o .:? "insert" .!= False
    enable <- o .:? "enable" .!= ""
    sync <- o .:? "sync" .!= True
    script <- T.lines <$> o .:? "script" .!= ""
    after <- T.lines <$> o .:? "after" .!= ""
    before <- T.lines <$> o .:? "before" .!= ""
    dependon <- o .: "dependon" <|> (fmap return <$> o .:? "dependon") .!= []
    dependedby <- o .: "dependedby" <|> (fmap return <$> o .:? "dependedby") .!= []
    loadafter <- o .: "loadafter" <|> (fmap return <$> o .:? "loadafter") .!= []
    loadbefore <- o .: "loadbefore" <|> (fmap return <$> o .:? "loadbefore") .!= []
    submodule <- o .:? "submodule" .!= False
    return Plugin {..}
