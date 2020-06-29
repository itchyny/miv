module Git
  ( clone
  , cloneSubmodule
  , pull
  , pullSubmodule
  , lastUpdate
  , gitStatus
  , gitUrl
  )
  where

import Data.List (isPrefixOf)
import System.Exit (ExitCode(..))
import System.Process (system, readProcess)

clone :: String -> FilePath -> String
clone repo path = unwords ["git", "clone", gitUrl repo, singleQuote path]

cloneSubmodule :: String -> FilePath -> String
cloneSubmodule repo path = unwords ["git", "clone", gitUrl repo, singleQuote path, "&&", "git", "-C", singleQuote path, "submodule", "update", "--init", "--recursive"]

pull :: FilePath -> String
pull path = unwords ["git", "-C", singleQuote path, "pull", "--rebase", "--prune"]

pullSubmodule :: FilePath -> String
pullSubmodule path = unwords ["git", "-C", singleQuote path, "pull", "--rebase", "--prune", "&&", "git", "-C", singleQuote path, "submodule", "update", "--init", "--recursive"]

lastUpdate :: FilePath -> IO Integer
lastUpdate path = read <$> readProcess "sh" ["-c", unwords ["git", "-C", singleQuote path, "show", "-s", "--format=%ct", "2>/dev/null", "||", "echo", "0"]] []

gitStatus :: FilePath -> IO ExitCode
gitStatus path = system $ unwords ["sh", "-c", "\"git", "-C", singleQuote path, "status", ">/dev/null 2>&1\""]

gitUrl :: String -> String
gitUrl xs | "https://" `isPrefixOf` xs = xs
gitUrl xs = "https://github.com/" <> xs

singleQuote :: String -> String
singleQuote str = "'" <> str <> "'"
