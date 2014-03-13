{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Prelude hiding (readFile)
import Data.Maybe (catMaybes, listToMaybe)
import Data.ByteString (readFile)
import System.Directory
import Control.Applicative ((<$>))

import qualified Setting as S

expandHomeDirectory :: FilePath -> IO FilePath
expandHomeDirectory ('~':path) = fmap (++path) getHomeDirectory
expandHomeDirectory path = return path

getSettingFile :: IO (Maybe FilePath)
getSettingFile
  = listToMaybe . catMaybes <$>
        mapM f [ "~/.vimrc.yaml"
               , "~/.vim/.vimrc.yaml"
               , "~/vimrc.yaml"
               , "~/.vim/vimrc.yaml"
               ]
    where f path = expandHomeDirectory path
               >>= doesFileExist
               >>= \b -> return (if b then Just path else Nothing)

getSetting :: IO (Maybe S.Setting)
getSetting = fmap S.decodeSetting $ getSettingFile >>=
               maybe (return "") ((=<<) readFile . expandHomeDirectory)

main :: IO ()
main = return ()
