
module Ant.Rand where

import Prelude hiding (LT, GT)

import qualified Data.Vector.Strict as Vec
import System.Random.MWC
import Ant.Dna
import Ant.Scanner
import Ant.Board.Compass

randTarget :: GenIO -> IO Target
randTarget gen = do 
  n <- uniformR (0::Int, 1) gen
  pure $ 
    case n of
      0 -> Food
      _ -> Block 

randComparator :: GenIO -> IO Comparator
randComparator gen = do 
  n <- uniformR (0::Int, 1) gen
  pure $ 
    case n of
      0 -> LT
      _ -> GT

randCompass :: GenIO -> IO Compass 
randCompass gen = do
  n <- uniformR (0::Int, 7) gen
  pure $ 
    case n of
      0 -> N
      1 -> S
      2 -> E
      3 -> W
      4 -> NE
      5 -> NW
      6 -> SE
      _ -> SW
  
randScanner :: GenIO -> IO Scanner
randScanner gen = do
  target     <- randTarget gen
  compass    <- randCompass gen
  comparator <- randComparator gen
  -- WARNING: magic values.. no related to the board settings
  comparated <- uniformR (0, 30) gen 

  pure Scanner 
    { target     = target
    , compass    = compass
    , comparator = comparator
    , comparated = comparated }

randOp :: GenIO -> IO Op 
randOp gen = do
  n <- uniformR (0::Int, 2) gen
  pure $ 
    case n of
      0 -> And
      1 -> Or
      _ -> Not

randGene :: GenIO -> IO Gene
randGene gen = do
  b <- uniform gen
  if b 
    then Left  <$> randOp gen
    else Right <$> randScanner gen

randDecisionTree :: GenIO -> IO DecisionTree
randDecisionTree gen = do 
  t_length <- uniformR (10, 30) gen
  Vec.replicateM t_length (randGene gen)

randMind :: GenIO -> IO Mind
randMind gen = do 
  t1  <- randDecisionTree gen
  t2  <- randDecisionTree gen 

  pure Mind 
    { shouldKeepGoing = t1
    , shouldTurnLeft  = t2 }

  
  





-- TODO: ..?
-- instance Variate Op where
--   uniform :: PrimMonad m => Gen (PrimState m) -> m a 
--   uniform 




