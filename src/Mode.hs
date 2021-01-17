{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
module Mode where

import Data.Text (unpack)
import Data.YAML

import ShowText

data Mode = NormalMode
          | VisualSelectMode
          | VisualMode
          | SelectMode
          | InsertMode
          | CmdlineMode
          | LangArgMode
          | OperatorPendingMode
          | TerminalMode
          deriving (Eq, Ord)

instance ShowText Mode where
  show NormalMode = "n"
  show VisualSelectMode = "v"
  show VisualMode = "x"
  show SelectMode = "s"
  show InsertMode = "i"
  show CmdlineMode = "c"
  show LangArgMode = "l"
  show OperatorPendingMode = "o"
  show TerminalMode = "t"

instance FromYAML Mode where
  parseYAML = withStr "!!str" \case
    "n" -> return NormalMode
    "v" -> return VisualSelectMode
    "x" -> return VisualMode
    "s" -> return SelectMode
    "i" -> return InsertMode
    "c" -> return CmdlineMode
    "l" -> return LangArgMode
    "o" -> return OperatorPendingMode
    "t" -> return TerminalMode
    x   -> fail $ unpack $ "failed to parse mode: " <> x
