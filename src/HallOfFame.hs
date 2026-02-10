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

  t <- get
  let (Fen _ r) = minimum t
  table <- liftIO (newTVarIO t)
  ruler <- liftIO (newTVarIO r)
  -- c  <- liftIO newChan
  queue_inserterc <- liftIO newChan

  let table_agent = tableAgent table queue_inserterc ruler 0 0

  t0 <- liftIO . forkIO . void $ runStateT table_agent t
  t1 <- liftIO . forkIO . void $ spawnPointAgent ruler table queue_inserterc
  t2 <- liftIO . forkIO . void $ spawnPointAgent ruler table queue_inserterc
  -- t3 <- liftIO . forkIO . void $ spawnPointAgent ruler table queue_inserterc
  -- t4 <- liftIO . forkIO . void $ spawnCrossAgent ruler table queue_inserterc
  t5 <- liftIO . forkIO . void $ spawnCrossAgent ruler table queue_inserterc
  t6 <- liftIO . forkIO . void $ spawnCrossAgent ruler table queue_inserterc

  _ <- liftIO . forever $ do
    threadDelay 100_000
    t_ <- liftIO $ readTVarIO table
    print t_
    when (done $ Heap.maximum t_) $ do
      print "done"
      print (Heap.maximum t_)
      forM_ [t0,t1,t2,{-t3,t4,-} t5, t6] killThread
      exitSuccess

  liftIO exitFailure

tableAgent :: (Eq a, Gen a, Ord (Score a))
  => TVar (Table a (Score a))
  -> Chan (Fen a (Score a)) 
  -> TVar (Score a)          -- ruler
  -> Int                     -- count
  -> Int                     -- iters
  -> Pop a (Score a)

tableAgent table queuec ruler count iters = do
  t <- get
  let (Fen _ new_ruler) = minimum t
  liftIO . void . atomically $ swapTVar ruler new_ruler
  
  -- when True $ do
  when (count >= 5 || iters >= 100) . liftIO $ do
    void . atomically $ swapTVar table t

  fen@(Fen _ score) <- liftIO $ readChan queuec
  (Fen _ min_score) <- gets minimum
  if score > min_score then do
    unless (fen `Heap.elem` t) $ do
      modify (Heap.insert fen)
      modify (Heap.drop 1)

    tableAgent table queuec ruler (count + 1) (iters + 1)
  else
    tableAgent table queuec ruler count (iters + 1)

spawnPointAgent :: (Gen a, Ord (Score a))
  => TVar (Score a)           -- comparator
  -> TVar (Table a (Score a)) -- table TVar
  -> Chan (Fen a (Score a))   -- queue ~ inserter
  -> IO (ThreadId, ThreadId, Chan (Fen a (Score a)))

spawnPointAgent ruler t inserter_queue = do
  point_queue <- newChan
  t1 <- forkIO $ pointAgent t point_queue
  t2 <- forkIO $ queueServer ruler point_queue inserter_queue
  pure (t1, t2, point_queue)

spawnCrossAgent :: (Gen a, Ord (Score a))
  => TVar (Score a)           -- comparator
  -> TVar (Table a (Score a)) -- table TVar
  -> Chan (Fen a (Score a))   -- queue ~ inserter
  -> IO (ThreadId, ThreadId, Chan (Fen a (Score a)))

spawnCrossAgent ruler t inserter_queue = do
  point_queue <- newChan
  t1 <- forkIO $ crossAgent t point_queue
  t2 <- forkIO $ queueServer ruler point_queue inserter_queue
  pure (t1, t2, point_queue)

queueServer :: (Gen a, Ord (Score a))
  => TVar (Score a)         -- comparator
  -> Chan (Fen a (Score a)) -- receive from 
  -> Chan (Fen a (Score a)) -- send to 
  -> IO ()

queueServer ruler agent inserter = do
  ruler_score <- readTVarIO ruler
  -- replicateM_ times $ do
  fen@(Fen _ score) <- readChan agent
  when (score > ruler_score) $
    writeChan inserter fen

  queueServer ruler agent inserter
  where times = 10_000

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


