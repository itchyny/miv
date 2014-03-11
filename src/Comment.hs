module Comment where

data Comment a = Comment a
               deriving Eq

instance Show a => Show (Comment a) where
  show (Comment a) = '"' : ' ' : show a

