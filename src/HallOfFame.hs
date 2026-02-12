{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RecordWildCards #-}

module HallOfFame where

import Gen

import Control.Monad
import Control.Monad.State

import qualified Data.Heap as Heap
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue
import Control.DeepSeq
import System.Exit

import qualified Data.Heap as Heap
import qualified Data.Vector.Strict as Vect
import qualified Data.Set as Set
import System.Random (randomRIO)
import qualified Data.Foldable as Heap hiding (minimum)

import qualified Data.List as List
import qualified Data.Ord

-- TODO: type families for vectors with exact size?
data Table a = Table
  { heap :: Heap.Heap (Indexed (Score a))
  , vect :: Vect.Vector a
  , set  :: Set.Set a
  } -- deriving (Show) 

tMin :: Table a -> Indexed (Score a)
tMin = Heap.minimum . heap

tMax :: (Eq (Score a), Ord (Score a)) => Table a -> Indexed (Score a)
tMax = Heap.maximum . heap

idx :: Int -> Table a -> a
idx i = (Vect.! i) . vect

oneFrom :: TVar (Vect.Vector a) -> IO a
oneFrom t = do
  v <- readTVarIO t
  i <- randomRIO (0, Vect.length v - 1)
  pure (v Vect.! i)

twoFrom :: TVar (Vect.Vector a) -> IO (a, a)
twoFrom t = do
  v <- readTVarIO t
  i <- randomRIO (0, Vect.length v - 1)
  j <- randomRIO (0, Vect.length v - 1)
  pure (v Vect.! i, v Vect.! j)

newtype Indexed b = Indexed (Int, b)
  deriving (Eq)

instance Ord b => Ord (Indexed b) where
  Indexed (_, l) `compare` Indexed (_, r) = l `compare` r

build :: forall a. (Gen a, Ord a, Ord (Score a)) => Vect.Vector a -> Table a
build vec =
  let !fens = (fit <$> Vect.toList vec)
      !(gs, ss) = unzip $ (\(Fen g s) -> (g, s)) <$> fens
      !i_ss::[Indexed (Score a)] = Indexed <$> zip [0..] ss

  in Table {
    heap = Heap.fromList i_ss,
    vect = vec,
    set  = Set.fromList gs
  }

insertAt :: (Gen a, Ord a, Ord (Score a)) => Fen a (Score a) -> Int -> Table a -> Table a
insertAt (Fen x score) min_idx t@Table {..}
  | x `Set.member` set = t
  | otherwise =
    let
      -- (!i, _) = Heap.minimum heap
      !val    = vect Vect.! min_idx
      !set'   = Set.delete val set
      !vect'  = vect Vect.// [(min_idx, x)]
      -- unsafe.. the index u passed have to be the min idx..
      !heap'  = Heap.adjustMin (\_ -> Indexed (min_idx, score)) heap

    in Table {
      heap = heap',
      vect = vect',
      set  = Set.insert x set'
    }

type Pop a = StateT (Table a) IO a

evolve :: forall a. (Eq a, Show a, Ord a, Gen a, Eq (Score a), Show (Score a), Ord (Score a)) => IO a
evolve = do
  let n = 200
  gs:: Vect.Vector a <- Vect.replicateM n new
  -- let xxx = Vect.head gs

  let table@Table {..} = build gs

  let Indexed (_, !r) = tMin table
  t_vect  <- liftIO (newTVarIO vect)
  t_ruler <- liftIO (newTVarIO r)
  -- c  <- liftIO newChan
  inserterq <- newTBQueueIO (fromIntegral (n `div` 2))

  let table_agent = tableAgent t_vect inserterq t_ruler 0 0

  t0 <- liftIO . forkIO . void $ runStateT table_agent table
  t1 <- liftIO . forkIO . void $ spawnPointAgent t_ruler t_vect inserterq
  t2 <- liftIO . forkIO . void $ spawnPointAgent t_ruler t_vect inserterq
  -- t3 <- liftIO . forkIO . void $ spawnPointAgent t_ruler t_vect inserterq
  -- t4 <- liftIO . forkIO . void $ spawnCrossAgent t_ruler t_vect inserterq
  t5 <- liftIO . forkIO . void $ spawnCrossAgent t_ruler t_vect inserterq
  t6 <- liftIO . forkIO . void $ spawnCrossAgent t_ruler t_vect inserterq

  t3 <- liftIO . forkIO . void $ spawnPointAgent t_ruler t_vect inserterq
  t3 <- liftIO . forkIO . void $ spawnPointAgent t_ruler t_vect inserterq
  -- t4 <- liftIO . forkIO . void $ spawnCrossAgent t_ruler t_vect inserterq
  -- t4 <- liftIO . forkIO . void $ spawnCrossAgent t_ruler t_vect inserterq
  -- t3 <- liftIO . forkIO . void $ spawnPointAgent t_ruler t_vect inserterq
  -- t4 <- liftIO . forkIO . void $ spawnCrossAgent t_ruler t_vect inserterq

  _ <- liftIO . forever $ do
    threadDelay 500_000
    l <- fmap fit . Vect.toList <$> readTVarIO t_vect
    let rv@(best:_) = List.sortOn (Data.Ord.Down . (\(Fen _ b) -> b)) l
    -- let best = head . reverse $ List.sortOn (\(Fen _ b) -> b) l
    print . take 20 $ fmap (\(Fen _ b) -> b) rv
    putStrLn "----------------------------------------------------"
    when (done best) $ do
      print "done"
      print best
      forM_ [t0,t1,{-t2,{-t3,t4,-} t5, -}t6] killThread
      exitSuccess

  liftIO exitFailure

tableAgent :: (Eq a, Ord a, Gen a, Ord (Score a))
  => TVar (Vect.Vector a)
  -> TBQueue (Fen a (Score a))
  -> TVar (Score a)          -- ruler
  -> Int                     -- count
  -> Int                     -- iters
  -> Pop a

tableAgent tvect queuec ruler !count !iters
  -- | count >= 5 || iters >= 100 = do
  --     Table { vect } <- get
  --     liftIO . void . atomically $ swapTVar tvect vect  -- adjusts vect -- TODO: just insert the thing by using function instead of that???
  --     tableAgent tvect queuec ruler 0 0  

  | otherwise = do
  Table { vect } <- get
  liftIO . void . atomically $ swapTVar tvect vect  -- adjusts vect -- TODO: just insert the thing by using function instead of that???

  Indexed (i, !min_score) <- gets tMin
  fen@(Fen _ score) <- liftIO . atomically $ readTBQueue queuec
  if score > min_score then do
    modify $ insertAt fen i

    Indexed (_, new_ruler) <- gets tMin
    liftIO . void . atomically $ swapTVar ruler new_ruler -- adjusts ruler
    tableAgent tvect queuec ruler (count + 1) (iters + 1)
  else
    tableAgent tvect queuec ruler count (iters + 1)

spawnPointAgent :: (Gen a, Ord (Score a))
  => TVar (Score a)           -- comparator
  -> TVar (Vect.Vector a) -- table TVar
  -> TBQueue (Fen a (Score a))   -- queue ~ inserter
  -> IO (ThreadId, ThreadId, TBQueue (Fen a (Score a)))

spawnPointAgent ruler t inserter_queue = do
  point_queue <- atomically $ newTBQueue 400
  t1 <- forkIO $ pointAgent t point_queue
  t2 <- forkIO $ queueServer ruler point_queue inserter_queue
  pure (t1, t2, point_queue)

spawnCrossAgent :: (Gen a, Ord (Score a))
  => TVar (Score a)            -- comparator
  -> TVar (Vect.Vector a)      -- table TVar
  -> TBQueue (Fen a (Score a)) -- queue ~ inserter
  -> IO (ThreadId, ThreadId, TBQueue (Fen a (Score a)))

spawnCrossAgent ruler t inserter_queue = do
  point_queue <- atomically $ newTBQueue 400
  t1 <- forkIO $ crossAgent t point_queue
  t2 <- forkIO $ queueServer ruler point_queue inserter_queue
  pure (t1, t2, point_queue)

queueServer :: (Gen a, Ord (Score a))
  => TVar (Score a)            -- comparator
  -> TBQueue (Fen a (Score a)) -- receive from 
  -> TBQueue (Fen a (Score a)) -- send to 
  -> IO ()

queueServer ruler agent inserter = do
  ruler_score       <- readTVarIO ruler
  fen@(Fen _ score) <- atomically $ readTBQueue agent
  -- when (score > ruler_score) $
  --   atomically $ writeTBQueue inserter $! fen
  atomically $ writeTBQueue inserter $! fen

  queueServer ruler agent inserter

pointAgent :: (Gen a, Ord (Score a))
  => TVar (Vect.Vector a)
  -> TBQueue (Fen a (Score a))
  -> IO ()
pointAgent tvect c = do
  g <- oneFrom tvect
  p <- point g
  atomically $ writeTBQueue c $! fit p

  pointAgent tvect c

crossAgent :: (Gen a, Ord (Score a))
  => TVar (Vect.Vector a)
  -> TBQueue (Fen a (Score a))
  -> IO ()
crossAgent tvect c = do
  (gL, gR) <- twoFrom tvect
  g <- cross gL gR
  atomically $ writeTBQueue c $! fit g

  crossAgent tvect c


