{-# LANGUAGE RecordWildCards #-}

module Ant.World.Screen where

import qualified Data.Vector.Strict as Vec

import Ant.Board.Pos2D

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Simulate
import qualified Data.List as List
import Ant.Board.Dir (Dir (Up))
import qualified Ant.World.State as World
import qualified Ant.World.Flow as World.Flow
import Ant.Dna (Mind(..))
import qualified System.Random.MWC as MWC
import GHC.IO (unsafePerformIO)
import Ant.Rand

bounds :: Int
bounds = 30

blocks' ::[Pos2D]
blocks' = List.nub 
        $ [Pos2D (i, 0)| i <- [0..bounds]]
       ++ [Pos2D (0, i)| i <- [0..bounds]]
       ++ [Pos2D (i, bounds)| i <- [0..bounds]]
       ++ [Pos2D (bounds, i)| i <- [0..bounds]]

initialState :: World.State
initialState =
  let ant = World.Ant 
        { ant_pos = Pos2D (bounds`div`2, bounds`div`2)
        , ant_score   = 0
        , ant_steps   = 0
        , ant_mind    = unsafePerformIO randMind
        , current_dir = Up } -- TODO: not randomiztn
      blocks_ = blocks'
  in World.State 
    { status = World.Running
    , player = ant
    , blocks = Vec.fromList blocks_
    , foods  = Vec.empty 
    , gen    = unsafePerformIO MWC.create }


tileSize :: Float
tileSize = 5--px 

modelPainter :: World.State -> IO Picture
modelPainter World.State {..} = do
  let blocks' = putTile white <$> blocks 
      foods'  = putTile blue  <$> foods
      ant     = putTile red    $  World.ant_pos player 

  pure . pictures $
    Vec.toList blocks' 
    ++ Vec.toList foods'
    ++ [ant]
  where 
    putTile c (Pos2D (x, y)) =
      color c . translate (x'*tileSize) (y'*tileSize)
        $ rectangleSolid tileSize tileSize
      where x' = fromIntegral x
            y' = fromIntegral y

-- (ViewPort -> Float -> model -> IO model)	
updateModel :: ViewPort -> Float -> World.State -> IO World.State
updateModel _ _ = World.Flow.step

runTest = simulateIO FullScreen black 60 initialState modelPainter updateModel


-- -> (model -> Picture)	
-- A function to convert the model to a picture.

-- -> (ViewPort -> Float -> model -> model)
