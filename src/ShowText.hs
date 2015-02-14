module ShowText where

import Data.Text

class ShowText a where
  show :: a -> Text
