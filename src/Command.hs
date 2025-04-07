module Command where

import Data.Default (Default(..))
import Data.Text (Text, null, unwords)
import Data.Text.Builder.Linear qualified as Builder
import Data.Text.Display (Display(..), display)
import Prelude hiding (unwords, null)

data Command =
  Command {
    name     :: Text,
    repl     :: Text,
    bang     :: Bool,
    bar      :: Bool,
    register :: Bool,
    buffer   :: Bool,
    range    :: Maybe CmdRange,
    arg      :: CmdArg,
    complete :: Maybe CmdComplete
  } deriving Eq

instance Display Command where
  displayBuilder cmd =
    Builder.fromText $ unwords $ filter (not . null)
        [ "command!",
          if cmd.bang then "-bang" else "",
          if cmd.bar then "-bar" else "",
          if cmd.register then "-register" else "",
          if cmd.buffer then "-buffer" else "",
          maybe "" display cmd.range,
          display cmd.arg,
          maybe "" display cmd.complete,
          cmd.name,
          cmd.repl ]

instance Default Command where
  def = Command {
    name     = "",
    repl     = "",
    bang     = True,
    bar      = False,
    register = False,
    buffer   = False,
    range    = Just CmdRange,
    arg      = CmdNonNegArg,
    complete = Nothing
  }

data CmdRange = CmdRange
              | CmdRangeWhole
              | CmdRangeN Int
              | CmdRangeCount Int
              deriving Eq

instance Display CmdRange where
  displayBuilder = Builder.fromText . \case
    CmdRange        -> "-range"
    CmdRangeWhole   -> "-range=%"
    CmdRangeN n     -> "-range=" <> display n
    CmdRangeCount n -> "-count=" <> display n

data CmdArg = CmdNonNegArg
            | CmdZeroOneArg
            | CmdPositiveArg
            | CmdOneArg
            | CmdNoArg
            deriving Eq

instance Display CmdArg where
  displayBuilder = Builder.fromText . \case
    CmdNonNegArg   -> "-nargs=*"
    CmdZeroOneArg  -> "-nargs=?"
    CmdPositiveArg -> "-nargs=+"
    CmdOneArg      -> "-nargs=1"
    CmdNoArg       -> "-nargs=0"

newtype CmdComplete = CmdComplete Text
                    deriving Eq

instance Display CmdComplete where
  displayBuilder (CmdComplete complete) =
    Builder.fromText $ "-complete=" <> complete
