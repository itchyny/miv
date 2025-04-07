module Cmdline where

import Data.Text (Text, unpack)
import Data.Text.Builder.Linear qualified as Builder
import Data.Text.Display (Display(..))
import Data.YAML

data Cmdline = CmdlineExCommand
             | CmdlineForwardSearch
             | CmdlineBackwardSearch
             | CmdlineInput
             deriving (Eq, Ord)

instance Display Cmdline where
  displayBuilder = Builder.fromText . \case
    CmdlineExCommand      -> ":"
    CmdlineForwardSearch  -> "/"
    CmdlineBackwardSearch -> "?"
    CmdlineInput          -> "@"

instance FromYAML Cmdline where
  parseYAML = withStr "!!str" \case
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
