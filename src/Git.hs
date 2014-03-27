module Git
  ( clone
  , pull
  , lastUpdate
  , gitUrl
  )
  where

import Data.Functor
import System.Process
import System.Exit

clone :: String -> String -> IO ExitCode
clone repo path = system $ unwords ["git", "clone", gitUrl repo, singleQuote path]

pull :: String -> IO ExitCode
pull path = system $ unwords ["cd", singleQuote path, "&&", "git", "pull", "--rebase"]

lastUpdate :: String -> IO Integer
lastUpdate path = read <$> readProcess "sh" ["-c", unwords ["cd", singleQuote path, "&&", "git", "show", "-s", "--format=%ct"]] []

gitUrl :: String -> String
gitUrl repo = "https://github.com/" ++ repo

singleQuote :: String -> String
singleQuote str = "'" ++ str ++ "'"
