{-# LANGUAGE StarIsType #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Gen where

class Gen a where
  type Score a
  fit   :: a -> Fen a (Score a)

  new   :: IO a
  point :: a -> IO a
  cross :: a -> a -> IO a

  done :: Fen a (Score a) -> Bool

data Fen a b = Fen a b deriving (Eq, Show)

instance (Eq a, Ord b) => Ord (Fen a b) where
  (Fen _ s) `compare` (Fen _ v) = s `compare` v


