module Git
  ( clone
  , cloneSubmodule
  , pull
  , pullSubmodule
  , lastUpdate
  , gitUrl
  )
  where

import Data.Functor ((<$>))
import System.Process (readProcess, system)
import System.Exit (ExitCode(..))

clone :: String -> String -> IO ExitCode
clone repo path = system $ unwords ["git", "clone", gitUrl repo, singleQuote path]

cloneSubmodule :: String -> String -> IO ExitCode
cloneSubmodule repo path = system $ unwords ["git", "clone", gitUrl repo, singleQuote path, "&&", "cd", singleQuote path, "&&", "git", "submodule", "update", "--init", "--recursive"]

pull :: String -> IO ExitCode
pull path = system $ unwords ["cd", singleQuote path, "&&", "git", "pull", "--rebase"]

pullSubmodule :: String -> IO ExitCode
pullSubmodule path = system $ unwords ["cd", singleQuote path, "&&", "git", "pull", "--rebase", "&&", "git", "submodule", "update", "--init", "--recursive"]

lastUpdate :: String -> IO Integer
lastUpdate path = read <$> readProcess "sh" ["-c", unwords ["cd", singleQuote path, "&&", "git", "show", "-s", "--format=%ct"]] []

gitUrl :: String -> String
gitUrl repo = "https://github.com/" ++ repo

singleQuote :: String -> String
singleQuote str = "\"" ++ str ++ "\""
