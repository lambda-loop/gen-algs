{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Ant.Instance where
import Genome
import Ant.Dna
import qualified System.Random.MWC as MWC
import Ant.Rand
import Mutation.Vec 
import System.Random.MWC (createSystemSeed)
import Ant.World.Screen
import Ant.World.Flow

import Control.Monad

import Ant.World.State
import Control.Concurrent.STM (TVar)
import qualified Data.Vector.Strict as Vec
import Ant.Feedback
import Graphics.Gloss.Interface.IO.Simulate
-- import qualified Data.Set as Set

instance Genome Mind where
  type (Score Mind) = Integer

  fit :: Mind -> IO (Fen Mind (Score Mind))
  fit mind = do
    gen <- MWC.createSystemRandom
    -- scores <- replicateM 1 $ do 
    s   <- initialState gen mind
    s'  <- playWholeGame s
    
    let steps   = (ant_steps . player) s'
        explr   = (explored_set . player) s'
        score_s = toInteger $ (steps * length explr) + (2 ^ score s')
    pure $ Fen ((ant_mind . player) s') score_s

    -- let rv@(best:_) = List.sortOn (Data.Ord.Down . (\(Fen _ b) -> b)) (scores)
    --
    -- pure best


  -- dont care for a ending o.O
  done :: Fen Mind (Score Mind) -> Bool
  done = const False

  new :: IO Mind
  new = randMind =<< MWC.createSystemRandom 

  point :: Mind -> MWC.GenIO -> IO Mind
  point mind@Mind {..} gen = do
    n::Int <- MWC.uniform gen
    case n of 
      -- left
      0 -> do
        l <- defaultSingle shouldKeepGoing gen (randGene gen)
        pure mind { shouldKeepGoing = l } 
      -- right
      1 -> do
        r <- defaultSingle shouldKeepGoing gen (randGene gen)
        pure mind { shouldTurnLeft = r } 
      -- both
      _ -> do
        l <- defaultSingle shouldKeepGoing gen (randGene gen)
        r <- defaultSingle shouldKeepGoing gen (randGene gen)
        pure mind 
          { shouldKeepGoing = l 
          , shouldTurnLeft  = r } 

  cross :: Mind -> Mind -> MWC.GenIO -> IO Mind
  cross mindl mindr gen = do
    n::Int <- MWC.uniform gen
    case n of 
      -- left
      0 -> do
        b <- MWC.uniform gen
        let mind = if b then mindl else mindr

        l <- altZipMin 
          (shouldKeepGoing mindl) 
          (shouldKeepGoing mindr) gen

        pure mind { shouldKeepGoing = l } 
      -- right
      1 -> do
        b <- MWC.uniform gen
        let mind = if b then mindl else mindr

        r <- altZipMin 
          (shouldTurnLeft mindl) 
          (shouldTurnLeft mindr) gen

        pure mind { shouldTurnLeft = r } 
      -- both
      _ -> do
        l <- altZipMin 
          (shouldKeepGoing mindl) 
          (shouldKeepGoing mindr) gen
        r <- altZipMin 
          (shouldTurnLeft mindl) 
          (shouldTurnLeft mindr) gen
        pure Mind
          { shouldKeepGoing = l 
          , shouldTurnLeft  = r } 
  
  -- type (Follower Mind) = TVar (Vec.Vector Mind)
  feedbacker :: IO (Vec.Vector Mind) -> IO ()
  feedbacker tVec = do
    let model₀ = mkModel tVec
    simulateIO FullScreen black 120
      model₀ painter updater
