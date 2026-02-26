
module Ant.Dna where

import qualified Data.Vector.Strict as Vec
import Ant.Scanner (Scanner)

data Op 
  = And
  | Or
  | Not
  deriving (Show, Eq, Ord)

type Gene = (Either Op Scanner)
type DecisionTree = Vec.Vector Gene
data Mind = Mind
  { shouldKeepGoing :: DecisionTree
  , shouldTurnLeft  :: DecisionTree

  } deriving (Show, Eq, Ord)




