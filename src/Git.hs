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
import Data.Text (Text, unwords, unpack)

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

gitUrl :: Text -> Text
gitUrl = ("https://github.com/" <>)

singleQuote :: Text -> Text
singleQuote str = "'" <> str <> "'"

system :: Text -> IO ExitCode
system = SP.system . unpack

readProcess :: FilePath -> [Text] -> String -> IO String
readProcess x y = SP.readProcess x (map unpack y)
