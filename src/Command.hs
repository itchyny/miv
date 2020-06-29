{-# LANGUAGE OverloadedStrings #-}
module Command where

import qualified Data.Text as T
import Data.Text (Text, unwords)
import Prelude hiding (show, unwords)
import ShowText

data CmdBang = CmdBang
             | CmdNoBang
             deriving Eq

instance ShowText CmdBang where
  show CmdBang = "-bang"
  show CmdNoBang = ""

data CmdBar = CmdBar
            | CmdNoBar
            deriving Eq

instance ShowText CmdBar where
  show CmdBar = "-bar"
  show CmdNoBar = ""

data CmdRegister = CmdRegister
                 | CmdNoRegister
                 deriving Eq

instance ShowText CmdRegister where
  show CmdRegister = "-register"
  show CmdNoRegister = ""

data CmdBuffer = CmdBuffer
               | CmdNoBuffer
               deriving Eq

instance ShowText CmdBuffer where
  show CmdBuffer = "-buffer"
  show CmdNoBuffer = ""

data CmdRange = CmdRange
              | CmdRangeWhole
              | CmdRangeN Int
              | CmdRangeCount Int
              | CmdNoRange
              deriving Eq

instance ShowText CmdRange where
  show CmdRange = "-range"
  show CmdRangeWhole = "-range=%"
  show (CmdRangeN n) = "-range=" <> show n
  show (CmdRangeCount n) = "-count=" <> show n
  show CmdNoRange = ""

data CmdArg = CmdNonNegArg
            | CmdZeroOneArg
            | CmdPositiveArg
            | CmdOneArg
            | CmdNoArg
            deriving Eq

instance ShowText CmdArg where
  show CmdNonNegArg = "-nargs=*"
  show CmdZeroOneArg = "-nargs=?"
  show CmdPositiveArg = "-nargs=+"
  show CmdOneArg = "-nargs=1"
  show CmdNoArg = "-nargs=0"

data CmdComplete = CmdComplete Text
                 deriving Eq

instance ShowText CmdComplete where
  show (CmdComplete "") = ""
  show (CmdComplete complete) = "-complete=" <> complete

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

instance ShowText Command where
  show cmd = unwords (filter (not . T.null)
           [ "command!"
           , show (cmdBang cmd)
           , show (cmdBar cmd)
           , show (cmdRegister cmd)
           , show (cmdBuffer cmd)
           , show (cmdRange cmd)
           , show (cmdArg cmd)
           , show (cmdComplete cmd)
           , cmdName cmd
           , cmdRepText cmd
           ])

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

