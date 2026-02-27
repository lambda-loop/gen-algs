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
    let chances = 3
    scores <- replicateM chances $ do 
      s   <- initialState gen mind
      s'  <- playWholeGame s
      
      let steps   = (ant_steps . player) s'
          explr   = (explored_set . player) s'
          score_s = toInteger $ length explr + (2 ^ score s')
      pure score_s

    let average_score = sum scores `div` fromIntegral chances
    pure (Fen mind average_score)

    -- let rv@(best:_) = List.sortOn (Data.Ord.Down . (\(Fen _ b) -> b)) (scores)
    --
    -- pure best

  done :: Fen Mind (Score Mind) -> Bool
  done (Fen _ n) = n > 700

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
  feedbacker a_vec = do
    gen <- MWC.createSystemRandom
    vec <- a_vec
    states <- mapM (initialState gen) vec
    results <- mapM playWholeGame states
    let scores = score <$> results
    print scores
    
    -- _ <- forM results $ \s -> 
    --   pure $ score s
      
    

    -- Vec.forM 
    -- model₀ <- mkModel tVec
    -- simulateIO FullScreen black 360
    --   model₀ painter updater
