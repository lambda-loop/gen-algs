{-# LANGUAGE TypeFamilies #-}
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


main :: IO ()
main = do
  a <- evalStateT evolve (Heap.empty :: Heap.Heap (Fen Word Int))
  print "hello, world"

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

answer = Vec.fromList "hello word"

wordFit :: Word -> Int
wordFit (Word code) = 
  Vec.sum $ Vec.zipWith aux code answer
  where 
    aux x y
      | x == y    = 1
      | otherwise = 0

