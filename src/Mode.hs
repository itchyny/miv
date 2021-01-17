{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Mode where

import Data.Text (unpack)
import Data.YAML

import ShowText

data Mode = NormalMode
          | VisualMode
          | SelectMode
          | InsertMode
          | CmdlineMode
          | ExMode
          | OperatorPendingMode
          | ReplaceMode
          | VirtualReplaceMode
          | InsertNormalMode
          | InesrtVisualMode
          | InsertSelectMode
          deriving (Eq, Ord)

instance ShowText Mode where
  show NormalMode = "n"
  show VisualMode = "v"
  show SelectMode = "s"
  show InsertMode = "i"
  show CmdlineMode = "c"
  show ExMode = "ex"
  show OperatorPendingMode = "o"
  show ReplaceMode = "r"
  show VirtualReplaceMode = "vr"
  show InsertNormalMode = "in"
  show InesrtVisualMode = "iv"
  show InsertSelectMode = "is"

instance FromYAML Mode where
  parseYAML = withStr "!!str" $ \case
    "n"  -> return NormalMode
    "v"  -> return VisualMode
    "s"  -> return SelectMode
    "i"  -> return InsertMode
    "c"  -> return CmdlineMode
    "ex" -> return ExMode
    "o"  -> return OperatorPendingMode
    "r"  -> return ReplaceMode
    "vr" -> return VirtualReplaceMode
    "in" -> return InsertNormalMode
    "iv" -> return InesrtVisualMode
    "is" -> return InsertSelectMode
    x    -> fail $ unpack $ "failed to parse mode: " <> x
