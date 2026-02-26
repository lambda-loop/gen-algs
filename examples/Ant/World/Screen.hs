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
import Ant.Rand

bounds :: Int
bounds = 30

blocks' ::[Pos2D]
blocks' = List.nub 
        $ [Pos2D (i, 0)| i <- [0..bounds]]
       ++ [Pos2D (0, i)| i <- [0..bounds]]
       ++ [Pos2D (i, bounds)| i <- [0..bounds]]
       ++ [Pos2D (bounds, i)| i <- [0..bounds]]

initialState :: MWC.GenIO -> Mind -> IO World.State
initialState gen ant_mind = do
  -- ant_mind <- randMind 
  more_blocks <- Vec.replicateM 30 $ do
    i <- MWC.uniformR (1, 29) gen   -- WARNING: magic values
    j <- MWC.uniformR (1, 29) gen  -- WARNING: magic values
    pure $ Pos2D (i, j)
  let p = Pos2D (bounds`div`2, bounds`div`2)
  let ant = World.Ant 
        { ant_pos = p 
        -- , ant_score   = 0
        , ant_steps   = 0
        , ant_mind    = ant_mind
        , current_dir = Up } -- TODO: not randomiztn
      blocks_ = blocks'
  pure World.State 
    { status = World.Running
    , player = ant
    , score  = 0
    , blocks = Vec.filter (/= p) $ Vec.fromList blocks_ Vec.++ more_blocks
    , foods  = Vec.empty
    , gen    = gen }


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

runTest :: IO ()
runTest = do 
  gen <- MWC.createSystemRandom
  ant_mind <- randMind gen
  state₀ <- initialState gen ant_mind
  simulateIO FullScreen black 60 state₀ modelPainter updateModel


-- -> (model -> Picture)	
-- A function to convert the model to a picture.

-- -> (ViewPort -> Float -> model -> model)
