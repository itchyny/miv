{-# LANGUAGE OverloadedStrings #-}
module Git
  ( clone
  , cloneSubmodule
  , pull
  , pullSubmodule
  , lastUpdate
  , gitStatus
  , gitUrl
  , system
  )
  where

import Data.Monoid ((<>))
import Data.Text (Text, unwords, unpack)
import Prelude hiding (unwords)
import System.Exit (ExitCode(..))
import qualified System.Process as SP

clone :: Text -> Text -> IO ExitCode
clone repo path = system $ unwords ["git", "clone", gitUrl repo, singleQuote path]

cloneSubmodule :: Text -> Text -> IO ExitCode
cloneSubmodule repo path = system $ unwords ["git", "clone", gitUrl repo, singleQuote path, "&&", "cd", singleQuote path, "&&", "git", "submodule", "update", "--init", "--recursive"]

pull :: Text -> IO ExitCode
pull path = system $ unwords ["cd", singleQuote path, "&&", "git", "pull", "--rebase"]

pullSubmodule :: Text -> IO ExitCode
pullSubmodule path = system $ unwords ["cd", singleQuote path, "&&", "git", "pull", "--rebase", "&&", "git", "submodule", "update", "--init", "--recursive"]

lastUpdate :: Text -> IO Integer
lastUpdate path = read <$> readProcess "sh" ["-c", unwords ["cd", singleQuote path, "&&", "git", "show", "-s", "--format=%ct"]] []

gitStatus :: Text -> IO ExitCode
gitStatus path = system $ unwords ["cd", singleQuote path, ">/dev/null 2>&1", "&&", "git", "status", ">/dev/null 2>&1"]

gitUrl :: Text -> Text
gitUrl = ("https://github.com/" <>)

singleQuote :: Text -> Text
singleQuote str = "'" <> str <> "'"

system :: Text -> IO ExitCode
system = SP.system . unpack

readProcess :: FilePath -> [Text] -> String -> IO String
readProcess x y = SP.readProcess x (map unpack y)
