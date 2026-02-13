{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RecordWildCards #-}

module HallOfFame where

import Gen

import Control.Monad
import Control.Monad.State.Strict

import Control.Concurrent
import Control.Concurrent.STM
import System.Exit

import qualified Data.Heap as Heap
import qualified Data.Vector.Strict as Vect -- TODO: delete_it
import qualified Data.Vector.Strict.Mutable as MVec
import qualified Data.Set as Set
import qualified Data.Foldable as Heap hiding (minimum)

import qualified Data.List as List
import qualified Data.Ord
import qualified System.Random.MWC as MWC

-- TODO: type families for vectors with exact size?
data Table a = Table
  { heap :: Heap.Heap (Indexed (Score a))
  , vect :: Vect.Vector a
  , set  :: Set.Set a
  } -- deriving (Show) 

tMin :: Table a -> (Score a, Table a)
tMin t = 
  let Indexed (_, !min_score) = Heap.minimum (heap t)
  in (min_score, t)

tMax :: (Eq (Score a), Ord (Score a)) => Table a -> (Indexed (Score a), Table a)
tMax t = (Heap.maximum (heap t), t)

idx :: Int -> Table a -> a
idx i = (Vect.! i) . vect

-- N Times :P
singlesFrom :: Int -> TVar (Table a) -> MWC.GenIO -> IO (Vect.Vector a)
singlesFrom n t gen = do
  Table{ vect } <- readTVarIO t
  is <- Vect.replicateM n $ MWC.uniformR (0, Vect.length vect - 1) gen
  Vect.forM is $ \i -> 
    pure (vect Vect.! i)

-- N Times :P
pairsFrom :: Int -> TVar (Table a) -> MWC.GenIO -> IO (Vect.Vector (a, a))
pairsFrom n t gen = do
  Table{ vect } <- readTVarIO t
  is <- Vect.replicateM n $ MWC.uniformR (0, Vect.length vect - 1) gen
  js <- Vect.replicateM n $ MWC.uniformR (0, Vect.length vect - 1) gen
  Vect.forM (Vect.zip is js) $ \(i, j) -> 
    pure (vect Vect.! i, vect Vect.! j)

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

insert :: (Gen a, Ord a, Ord (Score a)) => Fen a (Score a) -> Table a -> Table a
insert (Fen x score) t@Table {..}
  | x `Set.member` set = t
  | otherwise =
    let
      Indexed (!min_idx, _) = Heap.minimum heap
      !val                  = vect Vect.! min_idx
      !set'                 = Set.delete val set
      !vect'                = vect Vect.// [(min_idx, x)]
      !heap'  = Heap.adjustMin (\_ -> Indexed (min_idx, score)) heap

    in Table {
      heap = heap',
      vect = vect',
      set  = Set.insert x set'
    }

type Pop a = StateT (Table a) IO a

evolve :: forall a. (Eq a, Show a, Ord a, Gen a, Eq (Score a), Show (Score a), Ord (Score a)) => IO a
evolve = do
  let n = 20 --100
  gs:: Vect.Vector a <- Vect.replicateM n new

  let table@Table { heap } = build gs

  let Indexed (_, !r) = Heap.minimum heap 
  t_ruler <- liftIO (newTVarIO r)
  t_table <- liftIO (newTVarIO table)
  inserterq <- newTQueueIO -- (fromIntegral 10_0000)

  let table_agent = tableAgent t_table t_ruler r inserterq 

  _t0 <- liftIO . forkIO $ table_agent 
  
  let threads = 4
  let batch_size = threads*n
  replicateM_ threads $ do 
      gen <- MWC.createSystemRandom
      forkIO $ pointAgent t_table t_ruler  inserterq gen batch_size
  
  replicateM_ threads $ do
      gen <- MWC.createSystemRandom
      forkIO $ crossAgent t_table t_ruler  inserterq gen batch_size

  _ <- liftIO . forever $ do
    -- threadDelay 1_000_000
    t <- readTVarIO t_table
    let l = (fmap fit . Vect.toList . vect) t
    let rv@(best:_) = List.sortOn (Data.Ord.Down . (\(Fen _ b) -> b)) l
    -- let best = head . reverse $ List.sortOn (\(Fen _ b) -> b) l
    print . take 20 $ fmap (\(Fen _ b) -> b) rv
    -- putStrLn $ "queue length: " ++ show q_len
    putStrLn $ "----------------------------------------------------"
    when (done best) $ do
      print "done"
      print best
      -- forM_ [t0,t1,{-t2,{-t3,t4,-} t5, -}t6] killThread
      exitSuccess

  exitFailure

tableAgent :: (Eq a, Ord a, Gen a, Ord (Score a))
  => TVar (Table a)             -- TVar (Vect.Vector a)
  -> TVar (Score a)             -- ruler
  -> Score a  
  -> TQueue (Fen a (Score a))
  -> IO ()

tableAgent t_table t_ruler ruler queue = do
  fen@(Fen _ !score) <- atomically (readTQueue queue)
  ruler' <- if score > ruler 
    then atomically $ do
      modifyTVar' t_table (insert fen)
      !new_min <- stateTVar t_table tMin
      writeTVar t_ruler new_min
      pure new_min

    else pure ruler

  tableAgent t_table t_ruler ruler' queue 

pointAgent :: (Gen a, Ord (Score a))
  => TVar (Table a)
  -> TVar (Score a)
  -> TQueue (Fen a (Score a))
  -> MWC.GenIO
  -> Int                        -- batch size
  -> IO ()
pointAgent t_table t_ruler queue gen batch_size = do
  gs <- singlesFrom batch_size t_table gen
  ps <- mapM (`point` gen) gs
  let !candidates = fmap fit ps

  let best@(Fen _ score) = 
        Vect.maximumBy (\(Fen _ l) (Fen _ r) -> compare l r) candidates

  !min_score <- readTVarIO t_ruler
  when (score > min_score) $ do
    atomically $ writeTQueue queue best

  pointAgent t_table t_ruler queue gen batch_size

crossAgent :: (Gen a, Ord (Score a))
  => TVar (Table a)
  -> TVar (Score a)
  -> TQueue (Fen a (Score a))
  -> MWC.GenIO
  -> Int                        -- batch size
  -> IO ()
crossAgent t_table t_ruler queue gen batch_size = do
  gs <- pairsFrom batch_size t_table gen
  ps <- mapM (\(l, r) -> cross l r gen) gs
  let !candidates = fmap fit ps

  let best@(Fen _ score) = 
        Vect.maximumBy (\(Fen _ a) (Fen _ b) -> compare a b) candidates

  !min_score <- readTVarIO t_ruler
  when (score > min_score) $ do
    atomically $ writeTQueue queue best

  crossAgent t_table t_ruler queue gen batch_size


