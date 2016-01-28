{-# LANGUAGE FlexibleInstances, IncoherentInstances, UndecidableInstances #-}
module ShowText where

import Data.Text

class ShowText a where
  show :: a -> Text

instance Show a => ShowText a where
  show = pack . Prelude.show
