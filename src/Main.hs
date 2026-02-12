{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
import Control.Concurrent (forkIO, threadDelay, Chan, newChan, writeChan, readChan)
import Control.Monad (forever)
import qualified Data.Vector as Vec

import Prelude hiding (Word)
import Control.Monad.ST
import qualified Data.Heap as Heap
import System.Random
import Gen
import HallOfFame
import Control.Monad.State (evalStateT)
import qualified Dna0.Repr as Dna0
import qualified Dna0.Fit as Dna0
import qualified Dna0.Mut as Dna0
import Data.Ord (Down (Down))

import Dna1.Repr 
import Dna2.Repr (Genome)


main :: IO ()
main = do
  -- a <- evalStateT evolve (Heap.empty :: Heap.Heap (Fen Word Int))
  -- a <- evalStateT evolve (Heap.empty :: Heap.Heap (Fen Dna0.Expr (Down Double)))
  -- a::FastTree <- evalStateT evolve (Heap.empty :: Heap.Heap (Fen FastTree (Down Double)))
  a <- evalStateT evolve (Heap.empty :: Heap.Heap (Fen Genome (Down Double)))
  print a

instance Gen Word where
  type Score Word = Int
  fit w = Fen w (wordFit w)
  new = newWord (Vec.length answer)
  point = Main.point
  cross = Main.cross 
  done (Fen (Word w) _) = w == answer


newtype Word = Word (Vec.Vector Char)
  deriving (Show, Eq)

cross :: Word -> Word -> IO Word
cross (Word codeL) (Word codeR) = do
  let size = min (Vec.length codeL) (Vec.length codeR)
  sides::Vec.Vector Bool <- Vec.replicateM size randomIO
  code <- Vec.iforM sides $ \i cond -> pure $
    if cond 
        then codeL Vec.! i 
      else codeR Vec.! i

  pure (Word code)
    

point :: Word -> IO Word
point (Word code) = do
  i <- randomRIO (0, Vec.length code - 1)
  c <- randomIO

  pure $ Word (code Vec.// [(i, c)])

newWord :: Int -> IO Word
newWord n = Word <$> 
  Vec.replicateM n randomIO

answer = Vec.fromList "hello dev"

wordFit :: Word -> Int
wordFit (Word code) = 
  Vec.sum $ Vec.zipWith aux code answer
  where 
    aux x y
      | x == y    = 1
      | otherwise = 0

instance Gen Dna0.Expr where
  type Score Dna0.Expr = Down Double
  fit e =  Fen e (Down $ maybe infinity snd $ Dna0.fitness e)
  point e = do 
    b <- randomIO
    (if b then Dna0.pointMut 
      else Dna0.subTreeMut) e
  cross eL eR = do 
    (eL', eR') <- Dna0.crossOverWith eL eR
    b <- randomIO
    pure $ if b then eL' else eR'

  new = Dna0.randomDnaWithDepth 6

  done (Fen _ b) = b == 0

  -- generators = pure $ \dnas -> do
  --   let couples = [(x, y) | x <- dnas, y <- dnas]
  --   pointeds <- Dna0.pointMut `mapM` dnas
  --   shaved   <- Dna0.subTreeMut `mapM` dnas
  --   children <- uncurry Dna0.crossOverWith `mapM` couples
  --
  --   pure $ pointeds ++ shaved ++ uncurry (++) (unzip children)

  -- random_new = Dna0.randomDnaWithDepth 12

infinity :: Double
infinity = 1/0



