module Git where

import Data.Functor
import System.Process
import System.Exit

clone :: String -> String -> IO ExitCode
clone repo path = system $ unwords ["git", "clone", gitUrl repo, path]

pull :: String -> IO ExitCode
pull path = system $ unwords ["cd", path, "&&", "git", "pull", "--rebase"]

lastUpdate :: String -> IO Integer
lastUpdate path = read <$> readProcess "sh" ["-c", unwords ["cd", path, "&&", "git", "show", "-s", "--format=%ct"]] []

gitUrl :: String -> String
gitUrl repo = "https://github.com/" ++ repo
