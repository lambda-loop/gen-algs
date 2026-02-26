module Ant.Scanner where

import Ant.Board.Compass -- (Compass)
import qualified Data.Vector.Strict as Vec
import Ant.Board.Pos2D 

import Prelude hiding (LT, GT)

data Target 
  = Food
  | Block 
  deriving (Show, Eq, Ord)

data Comparator 
  = LT
  | GT
  deriving (Show, Eq, Ord)

comp :: Comparator -> Double -> Double -> Bool
comp LT = (<)
comp GT = (>)

data Scanner = Scanner 
  { target     :: Target 
  , compass    :: Compass
  , comparator :: Comparator
  , comparated :: Int
  } deriving (Show, Eq, Ord)


-- TODO: needs MORE testing
scan :: Pos2D -> Vec.Vector Pos2D -> Compass -> Comparator -> Int -> Bool
scan p ps compass comparator comparated = 
  ps |> Vec.mapMaybe (\p' -> distanceToIn p (p', compass))
     |> Vec.any (\c -> comp comparator c (fromIntegral comparated))
  where (|>) = flip ($)

vTest :: Vec.Vector Pos2D
vTest = Vec.fromList 
  [ Pos2D (0, 0)
  , Pos2D (10, 0)
  , Pos2D (1, 1)
  , Pos2D(-1, 1)
  , Pos2D(0, 10)
  , Pos2D(0, 11)
  ]

fl p compass =  Vec.mapMaybe (\p' -> distanceToIn p (p', compass))




