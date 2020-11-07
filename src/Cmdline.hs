{-# LANGUAGE OverloadedStrings #-}
module Cmdline where

import Data.Aeson
import qualified Data.Aeson as Aeson
import Data.Hashable
import Data.Text (Text,unpack)
import Prelude hiding (show)
import qualified Prelude as Prelude

import ReadText
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

instance ReadText Cmdline where
  read ":" = CmdlineExCommand
  read "/" = CmdlineForwardSearch
  read "?" = CmdlineBackwardSearch
  read "@" = CmdlineInput
  read m   = error $ "unknown cmdline: " ++ unpack m

instance Hashable Cmdline where
  hashWithSalt a CmdlineExCommand      = a `hashWithSalt` (0 :: Int)
  hashWithSalt a CmdlineForwardSearch  = a `hashWithSalt` (1 :: Int)
  hashWithSalt a CmdlineBackwardSearch = a `hashWithSalt` (2 :: Int)
  hashWithSalt a CmdlineInput          = a `hashWithSalt` (3 :: Int)

instance FromJSON Cmdline where
  parseJSON (Aeson.String str)
    | unpack str == ":" = return CmdlineExCommand
    | unpack str == "/" = return CmdlineForwardSearch
    | unpack str == "?" = return CmdlineBackwardSearch
    | unpack str == "@" = return CmdlineInput
  parseJSON o = error $ "failed to parse cmdline: " <> Prelude.show o

cmdlinePattern :: Cmdline -> Text
cmdlinePattern CmdlineExCommand      = ":"
cmdlinePattern CmdlineForwardSearch  = "/"
cmdlinePattern CmdlineBackwardSearch = "\\?"
cmdlinePattern CmdlineInput          = "@"
