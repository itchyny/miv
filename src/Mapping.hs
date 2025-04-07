module Mapping where

import Data.Default (Default(..))
import Data.Text (Text, null, unwords)
import Data.Text.Builder.Linear qualified as Builder
import Data.Text.Display (Display(..), display)
import Prelude hiding (unwords, null)

import Mode

data Mapping =
  Mapping {
    name    :: Text,
    repl    :: Text,
    unique  :: Bool,
    silent  :: Bool,
    mode    :: Mode
  } deriving Eq

instance Display Mapping where
  displayBuilder m =
    Builder.fromText $ unwords $ filter (not . null)
        [ display m.mode <> "noremap",
          (if m.unique then "<unique>" else "")
       <> (if m.silent then "<silent>" else ""),
          m.name,
          m.repl
        ]

instance Default Mapping where
  def = Mapping {
    name    = "",
    repl    = "",
    unique  = True,
    silent  = True,
    mode    = NormalMode
  }
