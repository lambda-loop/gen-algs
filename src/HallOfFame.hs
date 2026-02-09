{-# LANGUAGE AllowAmbiguousTypes #-}


module HallOfFame where

import Gen


import Control.Monad
import Control.Monad.State

import qualified Data.Heap as Heap
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Foldable as Heap
import System.Exit

type Table a b = Heap.Heap (Fen a b)
type Pop a b = StateT (Table a b) IO a

evolve :: (Eq a, Show a, Gen a, Show (Score a), Ord (Score a)) => Pop a (Score a)
evolve = do
  let n = 100
  gs <- liftIO $ replicateM n new

  put Heap.empty
  forM_ gs $ \g -> do
    modify (Heap.insert $ fit g)

  t  <- get
  t' <- liftIO (newTVarIO t)
  c  <- liftIO newChan

  t1 <- liftIO . forkIO $ syncAgent t' c

  t2 <- liftIO . forkIO $ pointAgent t' c
  t3 <- liftIO . forkIO $ pointAgent t' c

  -- t4 <- liftIO . forkIO $ crossAgent t' c
  -- t5 <- liftIO . forkIO $ crossAgent t' c

  _ <- liftIO . forever $ do
    threadDelay 1_000_000
    t_ <- liftIO $ readTVarIO t'
    print t_
    when (done $ Heap.maximum t_) $ do
      print "done"
      print (Heap.maximum t_)
      forM_ [t1,t2,t3] killThread
      exitSuccess

  liftIO exitFailure

syncAgent :: (Eq a, Gen a, Ord b) => TVar (Table a b) -> Chan (Fen a b) -> IO ()
syncAgent table c = do
  t <- readTVarIO table

  replicateM_ 1000 $ do
    fen@(Fen _ v) <- readChan c
    let (Fen _ min_v) = minimum t

    when (v > min_v) $ do
      atomically . modifyTVar table $ Heap.insert fen
      atomically . modifyTVar table $ Heap.drop 1

  syncAgent table c

pointAgent :: (Gen a, Ord (Score a)) => TVar (Table a (Score a)) -> Chan (Fen a (Score a)) -> IO ()
pointAgent table c = do
  fens <- Heap.toUnsortedList <$> readTVarIO table
  forM_ fens $ \(Fen g _) -> do
    p <- point g
    writeChan c (fit p)

  pointAgent table c


crossAgent :: (Gen a, Ord (Score a)) => TVar (Table a (Score a)) -> Chan (Fen a (Score a)) -> IO ()
crossAgent table c = do
  fens <- Heap.toUnsortedList <$> readTVarIO table
  let pairs = (,) <$> fens <*> fens
  forM_ pairs $ \(Fen gL _, Fen gR _) -> do
    g <- cross gL gR
    writeChan c (fit g)

  crossAgent table c



