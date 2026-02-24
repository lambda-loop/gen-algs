
module Ant.Board.Dir where

import Prelude hiding (Left, Right)

data Dir 
  = Up
  | Left
  | Right
  | Down
  deriving (Show, Eq, Ord)

reverse :: Dir -> Dir 
reverse Up    = Down
reverse Down  = Up 
reverse Left  = Right
reverse Right = Left

