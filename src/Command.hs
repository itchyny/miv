module Command where

import Data.Text (Text, unwords)
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as Builder
import Data.Text.Display (Display(..), display)
import Prelude hiding (unwords)

data CmdBang = CmdBang
             | CmdNoBang
             deriving Eq

instance Display CmdBang where
  displayBuilder = Builder.fromText . \case
    CmdBang   -> "-bang"
    CmdNoBang -> ""

data CmdBar = CmdBar
            | CmdNoBar
            deriving Eq

instance Display CmdBar where
  displayBuilder = Builder.fromText . \case
    CmdBar   -> "-bar"
    CmdNoBar -> ""

data CmdRegister = CmdRegister
                 | CmdNoRegister
                 deriving Eq

instance Display CmdRegister where
  displayBuilder = Builder.fromText . \case
    CmdRegister   -> "-register"
    CmdNoRegister -> ""

data CmdBuffer = CmdBuffer
               | CmdNoBuffer
               deriving Eq

instance Display CmdBuffer where
  displayBuilder = Builder.fromText . \case
    CmdBuffer   -> "-buffer"
    CmdNoBuffer -> ""

data CmdRange = CmdRange
              | CmdRangeWhole
              | CmdRangeN Int
              | CmdRangeCount Int
              | CmdNoRange
              deriving Eq

instance Display CmdRange where
  displayBuilder = Builder.fromText . \case
    CmdRange        -> "-range"
    CmdRangeWhole   -> "-range=%"
    CmdRangeN n     -> "-range=" <> display n
    CmdRangeCount n -> "-count=" <> display n
    CmdNoRange      -> ""

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
  displayBuilder = Builder.fromText . \case
    CmdComplete "" -> ""
    CmdComplete complete -> "-complete=" <> complete

data Command =
     Command { cmdName     :: Text
             , cmdRepText  :: Text
             , cmdBang     :: CmdBang
             , cmdBar      :: CmdBar
             , cmdRegister :: CmdRegister
             , cmdBuffer   :: CmdBuffer
             , cmdRange    :: CmdRange
             , cmdArg      :: CmdArg
             , cmdComplete :: CmdComplete
     } deriving Eq

instance Display Command where
  displayBuilder cmd =
    Builder.fromText $ unwords $
      filter (not . T.null)
        [ "command!"
        , display (cmdBang cmd)
        , display (cmdBar cmd)
        , display (cmdRegister cmd)
        , display (cmdBuffer cmd)
        , display (cmdRange cmd)
        , display (cmdArg cmd)
        , display (cmdComplete cmd)
        , cmdName cmd
        , cmdRepText cmd
        ]

defaultCommand :: Command
defaultCommand
 = Command { cmdName     = ""
           , cmdRepText  = ""
           , cmdBang     = CmdBang
           , cmdBar      = CmdNoBar
           , cmdRegister = CmdNoRegister
           , cmdBuffer   = CmdNoBuffer
           , cmdRange    = CmdRange
           , cmdArg      = CmdNonNegArg
           , cmdComplete = CmdComplete ""
 }

