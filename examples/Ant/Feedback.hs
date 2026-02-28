
module Ant.Feedback where

import qualified Data.Vector.Strict.Mutable as MVec
import qualified Data.Vector.Strict as Vec
import qualified Ant.World.State as World
import Graphics.Gloss
import Ant.World.Screen
import Ant.Dna

import qualified System.Random.MWC as MWC
import Graphics.Gloss.Data.ViewPort (ViewPort)
import Ant.World.Flow

import qualified Data.List as List
import Control.Concurrent.STM
-- import Genome

data Model = Model 
  { sync   :: IO (Vec.Vector Mind)
  , actual :: MVec.IOVector World.State
  }

mkModel :: IO (Vec.Vector Mind) -> IO Model
mkModel a_minds = do
  minds  <- a_minds
  states <- Vec.forM minds setupMind
  states'<- Vec.thaw states
  pure Model 
    { sync   = a_minds
    , actual = states'
    }

statePainter :: Vec.Vector World.State -> IO Picture 
statePainter = pictureStates 

painter :: Model -> IO Picture
painter model = do 
  let mvec = actual model
  vec <- Vec.freeze mvec
  statePainter vec

updater :: ViewPort -> Float -> Model -> IO Model
updater _ _ model = do 
  let v = actual model
  b <- stillRunning v
  if b then do
    MVec.iforM_ v $ \i _ -> do
      MVec.modifyM v step i
    pure model
    -- v' <- Vec.forM v step
    -- pure model { actual = v'}
  else do
    minds  <- sync model
    states <- Vec.forM minds setupMind
    states'<- Vec.thaw states
    pure model { actual = states' }

  where 
    stillRunning v = do --Vec.any ((== World.Running) . World.status) 
      -- let max_rounds::Int = 1_000 -- WARNING: magic value
      -- BUG: will not work
      v' <- Vec.freeze v
      pure $ Vec.any ((== World.Running) . World.status) v'


      
      -- s <- v `MVec.read` 0
      -- let ant   = World.player s
      --     steps = World.ant_steps ant
      -- print steps
      -- pure (steps < 1_001)

-- -- TODO: additional informqation [Fen Mind Integer] 
-- stageGames :: StateT (Vec.Vector World.State) IO ()
-- stageGames = do
--   states <- get
--
--   -- WARNING: slow
--   states'<- Vec.forM states $ \s -> do
--     if World.status s == World.Over 
--         then pure s
--       else liftIO (step s)
--       
--   put states'
--   pics <- liftIO . pictureStates $ Vec.toList states'
--   undefined
--
--   -- WARNING: dammmm slow
--   when (Vec.any ((== World.Running) . World.status) states') 
--     stageGames 
--

-- WARNING:  AI Assisted
pictureStates :: Vec.Vector World.State -> IO Picture
pictureStates states = do
  pics <- Vec.forM states $ \state -> do
    modelPainter state

  let cols = 5 * 2   
      hSpacing = 200
      vSpacing = 200

      picsList = Vec.toList pics
      positioned = [ translate (fromIntegral (col * hSpacing)) 
                               (fromIntegral (row * vSpacing)) 
                               pic
                   | (idx, pic) <- zip [0..] picsList,
                     let row = idx `div` cols
                         col = idx `mod` cols
                   ]

  pure $ pictures positioned
--
--       
--       
--       
--
--
--     
--
-- -- modelPainter :: World.State -> IO Picture
-- -- modelPainter World.State {..} = do
-- --   let blocks' = putTile white <$> blocks 
-- --       foods'  = putTile blue  <$> foods
-- --       ant     = putTile red    $  World.ant_pos player 
-- --
-- --   pure . pictures $
-- --     Vec.toList blocks' 
-- --     ++ Vec.toList foods'
-- --     ++ [ant]
-- --   where 
-- --     putTile c (Pos2D (x, y)) =
-- --       color c . translate (x'*tileSize) (y'*tileSize)
-- --         $ rectangleSolid tileSize tileSize
-- --       where x' = fromIntegral x
-- --             y' = fromIntegral y
-- --   
-- --
-- --
-- --

setupMind :: Mind -> IO World.State
setupMind mind = do
  gen <- MWC.createSystemRandom
  initialState gen mind
