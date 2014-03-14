module Git where

import System.Process
import System.Exit

clone :: String -> String -> IO ExitCode
clone repo path = system $ unwords ["git", "clone", gitUrl repo, path]

pull :: String -> IO ExitCode
pull path = system $ unwords ["cd", path, "&&", "git", "pull", "--rebase"]

gitUrl :: String -> String
gitUrl repo = "https://github.com/" ++ repo
