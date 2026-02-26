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

data Ant = Ant
  { ant_pos     :: Pos2D
  , ant_score   :: Int
  , ant_steps   :: Int
  , ant_mind    :: Mind
  , current_dir :: Dir
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
  , gen    :: MWC.GenIO
  } -- deriving (Eq, Show)

