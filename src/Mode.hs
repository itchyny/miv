module Mode where

import Data.Text (unpack)
import Data.Text.Builder.Linear qualified as Builder
import Data.Text.Display (Display(..))
import Data.YAML

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

instance Display Mode where
  displayBuilder = Builder.fromText . \case
    NormalMode          -> "n"
    VisualSelectMode    -> "v"
    VisualMode          -> "x"
    SelectMode          -> "s"
    InsertMode          -> "i"
    CmdlineMode         -> "c"
    LangArgMode         -> "l"
    OperatorPendingMode -> "o"
    TerminalMode        -> "t"

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
