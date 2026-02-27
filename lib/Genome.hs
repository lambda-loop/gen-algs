{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Genome where

import qualified System.Random.MWC as MWC
import Control.Concurrent.STM (TVar)
import qualified Data.Vector.Strict as Vec

data Fen a b = Fen a b deriving (Eq, Show)
class Genome a where
  type Score a
  fit  :: a -> IO (Fen a (Score a))
  done :: Fen a (Score a) -> Bool

  new   :: IO a
  point :: a -> MWC.GenIO -> IO a 
  cross :: a -> a -> MWC.GenIO -> IO a

  -- type Follower a 
  -- WARNING: bad but works..
  feedbacker :: IO (Vec.Vector a) -> IO ()

instance (Eq a, Ord b) => Ord (Fen a b) where
  (Fen _ s) `compare` (Fen _ v) = s `compare` v

-- data Model a = Model 
--   { sync   :: TVar (Vec.Vector a)
--   , actual :: Vec.Vector a
--   } 


