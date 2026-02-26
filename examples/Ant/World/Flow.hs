{-# LANGUAGE RecordWildCards #-}

module Ant.World.Flow where

import qualified Ant.World.State as World (State (State))
import Ant.World.State hiding (State)
import Ant.World.Decision
import Ant.Board.Pos2D
import qualified Ant.Board.Dir as Dir
import Control.Monad
import qualified Data.Vector.Strict as Vec

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
        , ant_score   = ant_score 
        , ant_steps   = ant_steps + 1
        , ant_mind    = ant_mind
        , current_dir = dir' }

      status' | pos' `Vec.elem` blocks s = Over
              | otherwise  = status s 
        
      -- TODO: missing food processing
        -- if pos' `Vec.elem` foods
      in pure s { player = ant', status = status' }
  where  itsOver World.State {..} = status == Over

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
