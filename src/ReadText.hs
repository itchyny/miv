{-# LANGUAGE FlexibleInstances, IncoherentInstances, UndecidableInstances #-}
module ReadText where

import Data.Text

class ReadText a where
  read :: Text -> a

instance Read a => ReadText a where
  read = Prelude.read . unpack
