{-# LANGUAGE OverloadedStrings #-}
module Git
  ( clone
  , cloneSubmodule
  , pull
  , pullSubmodule
  , lastUpdate
  , gitUrl
  , system
  )
  where

import Prelude hiding (unwords)
import Data.Functor ((<$>))
import Data.Monoid ((<>))
import qualified System.Process as SP
import System.Exit (ExitCode(..))
import qualified Data.Text as T
import Data.Text (unwords)

clone :: T.Text -> T.Text -> IO ExitCode
clone repo path = system $ unwords ["git", "clone", gitUrl repo, singleQuote path]

cloneSubmodule :: T.Text -> T.Text -> IO ExitCode
cloneSubmodule repo path = system $ unwords ["git", "clone", gitUrl repo, singleQuote path, "&&", "cd", singleQuote path, "&&", "git", "submodule", "update", "--init", "--recursive"]

pull :: T.Text -> IO ExitCode
pull path = system $ unwords ["cd", singleQuote path, "&&", "git", "pull", "--rebase"]

pullSubmodule :: T.Text -> IO ExitCode
pullSubmodule path = system $ unwords ["cd", singleQuote path, "&&", "git", "pull", "--rebase", "&&", "git", "submodule", "update", "--init", "--recursive"]

lastUpdate :: T.Text -> IO Integer
lastUpdate path = read <$> readProcess "sh" ["-c", unwords ["cd", singleQuote path, "&&", "git", "show", "-s", "--format=%ct"]] []

gitUrl :: T.Text -> T.Text
gitUrl = ("https://github.com/" <>)

singleQuote :: T.Text -> T.Text
singleQuote str = "'" <> str <> "'"

system :: T.Text -> IO ExitCode
system = SP.system . T.unpack

readProcess :: FilePath -> [T.Text] -> String -> IO String
readProcess x y = SP.readProcess x (map T.unpack y)
