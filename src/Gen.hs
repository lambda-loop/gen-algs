{-# LANGUAGE StarIsType #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Gen where

class Gen a where
  type Score a
  fit   :: a -> (Fen a (Score a))

  -- TODO: implement it
  -- nularys :: [IO a]
  -- unarys  :: [a -> IO a]
  -- binarys :: [a -> a -> IO a]

  new   :: IO a
  point :: a -> IO a
  cross :: a -> a -> IO a

  -- in the snake case, the Fen is also important because 
  -- rerunning the fit could not lead to the same result
  done :: Fen a (Score a) -> Bool

data Fen a b = Fen a b deriving (Eq, Show)

instance (Eq a, Ord b) => Ord (Fen a b) where
  (Fen _ s) `compare` (Fen _ v) = s `compare` v


