module Strategy.IslandModel where

import Genome
import Strategy.HallOfFame hiding (evolve) 

import qualified Data.Heap as Heap
import qualified Data.Vector.Strict as Vec -- TODO: delete_it
import qualified Data.Vector.Strict.Mutable as MVec
import qualified Data.Set as Set
import qualified Data.Foldable as Heap hiding (minimum)

import qualified Data.List as List
import qualified Data.Ord
import qualified System.Random.MWC as MWC
import Control.Concurrent.STM
import Control.Monad
import Control.Concurrent
import System.Exit
import Control.Monad.State

evolve :: forall a. (Eq a, Show a, Ord a, Genome a, Eq (Score a), Show (Score a), Ord (Score a)) 
  => Int     -- ^. number of islands
  -> Int     -- ^. population per island
  -> Int     -- ^. agents pairs per island (threads/2)
  -> IO a    
evolve nI popI agentsI = do
  let queueB       = popI 
      batch_size   = popI
      travel_batch = popI 
      sub_elite    = popI `div` nI

  islands <- Vec.replicateM nI (setup popI)
  qs <- Vec.forM islands $ \(gs::Vec.Vector a) -> do
    queue <- newTBQueueIO (fromIntegral queueB)
    (agent@AgentCtx{snapshot, t_ruler}, t, r) <- mkAgentCtx queue gs
    void . forkIO $ tableAgent t snapshot t_ruler r queue 

    replicateM_ agentsI $ do 
      gen <- MWC.createSystemRandom
      forkIO $ pointAgent snapshot t_ruler queue gen batch_size

    replicateM_ agentsI $ do 
      gen <- MWC.createSystemRandom
      forkIO $ crossAgent snapshot t_ruler queue gen batch_size

    -- WARNING: in testing
    -- BUG: working for only the last snapshot
    pure agent

  -- master (travelAgent)
  gen <- MWC.createSystemRandom
  void . forkIO $ travelAgent qs gen travel_batch sub_elite

  let snapshots =  snapshot <$> qs
  -- closure? 👀
  let follower :: StateT (Vec.Vector a) IO (Vec.Vector a) = do
        forM_ snapshots $ \snap -> do
          v <- liftIO $ readTVarIO snap
          modify (Vec.++ v)
        get

  feedbacker (execStateT follower Vec.empty )

  let ags' = cycle (Vec.toList qs)
  -- sorry1 <- readTVarIO . snapshot . head $ ags'
  -- sorry2 <- newTVarIO (sorry1 Vec.! 0)

  forWhile ags' $ \ag -> do
    threadDelay 1_000_000
    let t_vect = snapshot ag

    t::Vec.Vector a <- readTVarIO t_vect 
    l::[Fen a (Score a)] <- (mapM fit . Vec.toList) t
    let rv@(best:_) = List.sortOn (Data.Ord.Down . (\(Fen _ b) -> b)) l
        Fen gene _ = best

    putStrLn $ "----------------------------------------------------"
    print . take 10 $ fmap (\(Fen _ b) -> b) rv
    if done best then pure (Just gene)
    else pure Nothing

  where
    -- TODO: iteration limits??
    forWhile :: [b] -> (b -> IO (Maybe c)) -> IO c
    forWhile [] _ = error "aaa sorry the clock iss.."
    forWhile (x:xs) f = do
      mx <- f x 
      case mx of
        Nothing -> forWhile (x:xs) f
        Just x' -> pure x'

spawnAgents :: undefined
spawnAgents = undefined

mkAgentCtx :: forall a. (Eq a, Show a, Ord a, Genome a, Eq (Score a), Show (Score a), Ord (Score a)) 
  => TBQueue (Fen a (Score a))
  -> Vec.Vector a -> IO (AgentCtx a, Table a, Score a)
mkAgentCtx queue v = do
  t@Table { heap } <- build v
  let Indexed (_, !r) = Heap.minimum heap 
  t_vec   <- newTVarIO v
  t_ruler <- newTVarIO r

  pure (AgentCtx queue t_vec t_ruler, t, r)


data AgentCtx a = AgentCtx 
  { queue    :: TBQueue (Fen a (Score a))  -- island
  , snapshot :: TVar (Vec.Vector a)        -- snapshot 
  , t_ruler  :: TVar (Score a)             -- ruler
  } 

pairsFrom' 
  :: Int 
  -> Vec.Vector a 
  -> Vec.Vector a 
  -> MWC.GenIO 
  -> IO (Vec.Vector (a, a))
pairsFrom' n vl vr gen = do
  let vl_len = Vec.length vl
  let vr_len = Vec.length vr

  is <- Vec.replicateM n $ MWC.uniformR (0, vl_len - 1) gen
  js <- Vec.replicateM n $ MWC.uniformR (0, vr_len - 1) gen
  pure $ Vec.zipWith (\i j -> (vl Vec.! i, vr Vec.! j)) is js

travelAgent :: (Genome a, Ord (Score a))
  => Vec.Vector (AgentCtx a)
  -> MWC.GenIO
  -> Int       -- batch size
  -> Int       -- sub elite
  -> IO ()
travelAgent agents_ctx gen batch_size sub_elite_size = do
  let range = [0..Vec.length agents_ctx - 1]
      is_js = [(i, j) | i <- range, j <- range]
  forM_ (cycle is_js) $ \(i, j) -> do
    let agent_l = agents_ctx Vec.! i
        agent_r = agents_ctx Vec.! j

    vl <- readTVarIO (snapshot agent_l)
    vr <- readTVarIO (snapshot agent_r)

    couples <- pairsFrom' batch_size vl vr gen
    couples'<- mapM (\(l, r) -> cross l r gen) couples

    !min_score <- readTVarIO (t_ruler agent_r)

    !candidates <- mapM fit couples' 
    let !approveds  = Vec.take sub_elite_size $ Vec.filter 
          (\(Fen _ s) -> s > min_score) 
          candidates 
    
    atomically $ forM_ approveds $ \fen@(Fen _ s) -> do 
      -- when (s > min_score) $ 
      writeTBQueue (queue agent_r) fen

  error "unreachable state in travel agent!"
  -- travelAgent agents_ctx gen batch_size sub_elite_size


