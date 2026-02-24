
module Ant.Dna where

import qualified Data.Vector.Strict as Vec
import Ant.Board.Compass (Compass)

data Op 
  = And
  | Or
  | Not
  deriving (Show, Eq, Ord)

data Target 
  = Food
  | Wall
  deriving (Show, Eq, Ord)

data Comparator 
  = LessT
  | GreaterT
  deriving (Show, Eq, Ord)

data Scanner = Scanner 
  { target     :: Target 
  , compass    :: Compass
  , comparator :: Comparator
  } deriving (Show, Eq, Ord)

data Gene = Either Op Scanner
  deriving (Show, Eq, Ord)

type DecisionTree = Vec.Vector Gene
data Mind = Mind
  { shouldKeepGoing :: DecisionTree
  , shouldTurnLeft  :: DecisionTree
  } deriving (Show, Eq, Ord)

-- Neovim




