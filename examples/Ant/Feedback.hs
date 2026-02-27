
module Ant.Feedback where

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
  , actual :: Vec.Vector World.State
  }

mkModel :: IO (Vec.Vector Mind) -> Model
mkModel a_vec = Model 
  { sync   = a_vec
  , actual = Vec.empty
  }

statePainter :: Vec.Vector World.State -> IO Picture 
statePainter = pictureStates 

painter :: Model -> IO Picture
painter = statePainter . actual

updater :: ViewPort -> Float -> Model -> IO Model
updater _ _ model = do 
  let v = actual model
  if stillRunning v then do
    v' <- Vec.forM v step
    pure model { actual = v'}
  else do
    minds  <- sync model
    states <- Vec.forM minds setupMind
    pure model { actual = states }

  where 
    stillRunning = Vec.any ((== World.Running) . World.status) 

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
