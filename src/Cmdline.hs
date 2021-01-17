{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Cmdline where

import Data.Text (Text, unpack)
import Data.YAML
import Prelude hiding (show)

import ShowText

data Cmdline = CmdlineExCommand
             | CmdlineForwardSearch
             | CmdlineBackwardSearch
             | CmdlineInput
             deriving (Eq, Ord)

instance ShowText Cmdline where
  show CmdlineExCommand      = ":"
  show CmdlineForwardSearch  = "/"
  show CmdlineBackwardSearch = "?"
  show CmdlineInput          = "@"

instance FromYAML Cmdline where
  parseYAML = withStr "!!str" $ \case
    ":" -> return CmdlineExCommand
    "/" -> return CmdlineForwardSearch
    "?" -> return CmdlineBackwardSearch
    "@" -> return CmdlineInput
    x   -> fail $ unpack $ "failed to parse cmdline: " <> x

cmdlinePattern :: Cmdline -> Text
cmdlinePattern CmdlineExCommand      = ":"
cmdlinePattern CmdlineForwardSearch  = "/"
cmdlinePattern CmdlineBackwardSearch = "\\?"
cmdlinePattern CmdlineInput          = "@"
