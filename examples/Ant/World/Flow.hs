{-# LANGUAGE RecordWildCards #-}

module Ant.World.Flow where

import qualified Ant.World.State as World (State (State))
import Ant.World.State hiding (State)
import Ant.World.Decision
import Ant.Board.Pos2D
import qualified Ant.Board.Dir as Dir
import Control.Monad
import qualified Data.Vector.Strict as Vec
import qualified System.Random.MWC as MWC

-- TODO: lenses when ;-;?
step :: World.State -> IO World.State
step s 
  | itsOver s = pure s
  | otherwise = do
    let Ant {..} = player s

    if ant_steps > 1_000 then 
      pure s { status = Over }
    else let 
      move = ant_mind `act` s
      (pos', dir') = (ant_pos, current_dir) `after` move
    
      ant' = Ant 
        { ant_pos     = pos'
        , ant_steps   = ant_steps + 1
        , ant_mind    = ant_mind
        , current_dir = dir' }

      status' | pos' `Vec.elem` blocks s = Over
              | otherwise = status s 
      fs = foods s
       
      -- TODO: missing food processing
        -- if pos' `Vec.elem` foods
      in do
      foods' <- do 
        if ant_steps `rem` 20 /= 0 then pure fs -- WARNING: magic values
        else Vec.snoc fs <$> foodGen fs (blocks s) pos' (gen s)

      let foods'' = Vec.filter (/= pos') foods'
          score'  
            | Vec.length foods' /= Vec.length foods'' = score s + 1
            | otherwise = score s

      pure s { player = ant', status = status', foods = foods'', score = score'}

  where  
    itsOver World.State {..} = status == Over
    foodGen :: Vec.Vector Pos2D -> Vec.Vector Pos2D -> Pos2D -> MWC.GenIO -> IO Pos2D
    foodGen fs bs ant_pos gen = do 
      pos' <- new_pos gen
      if pos' `Vec.elem` fs ||
         pos' `Vec.elem` bs || 
         pos' == ant_pos then foodGen fs bs ant_pos gen
      else pure pos'

    new_pos :: MWC.GenIO -> IO Pos2D -- WARNING: magic values
    new_pos gen = do 
      i <- MWC.uniformR (1, 29) gen
      j <- MWC.uniformR (1, 29) gen
      pure $ Pos2D (i, j)
      
      


after :: (Pos2D, Dir.Dir) -> Move -> (Pos2D, Dir.Dir)
(pos, dir) `after` move = 
  let 
    dir' = dir `adjustWith` move
    pos' = pos `steppingTo` dir'
  in 
    (pos', dir')

adjustWith :: Dir.Dir -> Move -> Dir.Dir
dir `adjustWith` KeepGoing = dir

-- Left
Dir.Up    `adjustWith` TurnLeft = Dir.Left
Dir.Left  `adjustWith` TurnLeft = Dir.Down
Dir.Down  `adjustWith` TurnLeft = Dir.Right
Dir.Right `adjustWith` TurnLeft = Dir.Up

-- Right
Dir.Up    `adjustWith` TurnRight = Dir.Right
Dir.Right `adjustWith` TurnRight = Dir.Down
Dir.Down  `adjustWith` TurnRight = Dir.Left 
Dir.Left  `adjustWith` TurnRight = Dir.Up

steppingTo :: Pos2D -> Dir.Dir -> Pos2D
steppingTo (Pos2D (x, y)) Dir.Up    = Pos2D (x, y+1)
steppingTo (Pos2D (x, y)) Dir.Left  = Pos2D (x-1, y) 
steppingTo (Pos2D (x, y)) Dir.Right = Pos2D (x+1, y) 
steppingTo (Pos2D (x, y)) Dir.Down  = Pos2D (x, y-1) 
