{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Prelude hiding (readFile)
import Data.Maybe (listToMaybe)
import Data.ByteString (readFile)
import System.Directory
import Control.Applicative ((<$>))
import Control.Monad (filterM)

import qualified Setting as S

expandHomeDirectory :: FilePath -> IO FilePath
expandHomeDirectory ('~':path) = fmap (++path) getHomeDirectory
expandHomeDirectory path = return path

getSettingFile :: IO (Maybe FilePath)
getSettingFile
  = listToMaybe <$> filterM ((=<<) doesFileExist . expandHomeDirectory)
       [ "~/.vimrc.yaml"
       , "~/.vim/.vimrc.yaml"
       , "~/vimrc.yaml"
       , "~/.vim/vimrc.yaml"
       ]

getSetting :: IO (Maybe S.Setting)
getSetting = fmap S.decodeSetting $ getSettingFile >>=
               maybe (return "") ((=<<) readFile . expandHomeDirectory)

main :: IO ()
main = return ()
