{-# LANGUAGE OverloadedStrings #-}
module Mode where

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

instance Read Mode where
  readsPrec _ "n"  = [(NormalMode, "")]
  readsPrec _ "v"  = [(VisualMode, "")]
  readsPrec _ "s"  = [(SelectMode, "")]
  readsPrec _ "i"  = [(InsertMode, "")]
  readsPrec _ "c"  = [(CmdlineMode, "")]
  readsPrec _ "ex" = [(ExMode, "")]
  readsPrec _ "o"  = [(OperatorPendingMode, "")]
  readsPrec _ "r"  = [(ReplaceMode, "")]
  readsPrec _ "vr" = [(VirtualReplaceMode, "")]
  readsPrec _ "in" = [(InsertNormalMode, "")]
  readsPrec _ "iv" = [(InesrtVisualMode, "")]
  readsPrec _ "is" = [(InsertSelectMode, "")]
  readsPrec _ _    = []
