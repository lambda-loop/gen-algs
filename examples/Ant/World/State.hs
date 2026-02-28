{-# LANGUAGE RecordWildCards #-}

module Ant.World.State where

import qualified Data.Vector.Strict as Vec
import Ant.Board.Pos2D
import Ant.Dna
-- import Graphics.Gloss
import Graphics.Gloss.Data.Color (black)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Simulate
import qualified Data.List as List
import qualified System.Random.MWC as MWC
import Ant.Board.Dir (Dir (Up))
import qualified Data.Set as Set

data Ant = Ant
  { ant_pos     :: Pos2D
  -- , ant_score   :: Int
  , ant_stamina :: Int
  , ant_steps   :: Int
  , ant_mind    :: Mind
  , current_dir :: Dir
  , explored_set:: Set.Set Pos2D
  } deriving (Eq, Show)

data Status
  = Running
  | Over
  deriving (Eq, Show, Ord)

data State = State
  { status :: Status
  , player :: Ant
  , blocks :: Vec.Vector Pos2D
  , foods  :: Vec.Vector Pos2D
  , score  :: Int
  , gen    :: MWC.GenIO
  } -- deriving (Eq, Show)


defaultStamina :: Int
defaultStamina = 450
