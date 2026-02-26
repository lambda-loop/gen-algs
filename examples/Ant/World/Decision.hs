{-# LANGUAGE RecordWildCards #-}

module Ant.World.Decision where
import Ant.Dna
import qualified Ant.World.State as World
import qualified Data.Vector.Strict as Vec
import Control.Monad.Identity
import Control.Monad
import Control.Monad.State hiding (state)
import Ant.Scanner

data Move 
  = TurnLeft
  | TurnRight
  | KeepGoing
  deriving (Eq, Show, Ord)

decide' :: DecisionTree -> World.State -> Bool
decide' tree state = evalState (decide tree state) []

decide :: DecisionTree -> World.State -> State [Bool] Bool
decide tree state = do
  Vec.forM_ tree $ \g -> do
    modify (step g)

  stack <- get
  pure $ case stack of 
          []    -> False 
          (b:_) -> b
  where
    step :: Gene -> [Bool] -> [Bool]
    step (Left And) []  = [False]
    step (Left And) [b] = [b]
    step (Left And) (bl:br:stack) = (bl && br) : stack

    step (Left Or) (bl:br:stack) = (bl || br) : stack
    step (Left Or) _ = [True]
      
    step (Left Not) [] = []
    step (Left Not) (b:stack) = not b : stack

    step (Right Scanner {..}) stack = 
      scan ant vec compass comparator comparated : stack       
      where 
        ant = (World.ant_pos . World.player) state
        vec = case target of
          Food  -> World.foods state
          Block -> World.blocks state
 
-- TODO: Scanners shouldent repeat themselfs..
-- WARNING: Slow implementation
act :: Mind -> World.State -> Move
act Mind {..} state
  | decide' shouldKeepGoing state = KeepGoing
  | decide' shouldTurnLeft state  = TurnLeft
  | otherwise                     = TurnRight


-- decide :: DecisionTree -> World.State -> State [Bool] Bool
