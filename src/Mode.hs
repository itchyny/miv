{-# LANGUAGE OverloadedStrings #-}
module Mode where

import Data.Text (unpack)

import ReadText
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
          deriving Eq

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

instance ReadText Mode where
  read "n"  = NormalMode
  read "v"  = VisualMode
  read "s"  = SelectMode
  read "i"  = InsertMode
  read "c"  = CmdlineMode
  read "ex" = ExMode
  read "o"  = OperatorPendingMode
  read "r"  = ReplaceMode
  read "vr" = VirtualReplaceMode
  read "in" = InsertNormalMode
  read "iv" = InesrtVisualMode
  read "is" = InsertSelectMode
  read m    = error $ "unknown mode: " ++ unpack m
