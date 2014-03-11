module Mode where

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

instance Show Mode where
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
