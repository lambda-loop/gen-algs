{-# LANGUAGE TypeFamilies #-}

module Dna2.Repr where

import Control.Monad
  
import qualified Gen 
import Data.Ord (Down (Down))
import System.Random
import Data.Map.Strict hiding (drop)
import Dna0.Fit (is)
import Gen (Fen(..))

data Gen 
  = Inp
  | Lit Int
  | Add
  | Mul
  | Sub 
  | Div 
  | Pow
  deriving (Show, Eq, Ord)

type Genome = [Gen]

foo :: Genome -> Int -> [Double] -> Double
foo [] _ []    = 0
foo [] _ (x:_) = x

foo (Inp:gs) inp stack   = foo gs inp (fromIntegral inp:stack)
foo (Lit x:gs) inp stack = foo gs inp (fromIntegral x:stack)

foo (Add:gs) inp []          = foo gs inp [0]
foo (Add:gs) inp [x]         = foo gs inp [x]
foo (Add:gs) inp (x:y:stack) = foo gs inp (y+x:stack)

foo (Sub:gs) inp []          = foo gs inp [0]
foo (Sub:gs) inp [x]         = foo gs inp [-x]
foo (Sub:gs) inp (x:y:stack) = foo gs inp (y-x:stack)

foo (Mul:gs) inp []          = foo gs inp [1]
foo (Mul:gs) inp [x]         = foo gs inp [x]
foo (Mul:gs) inp (x:y:stack) = foo gs inp (y*x:stack)

foo (Div:gs) inp []          = foo gs inp [1]
foo (Div:gs) inp [x]         = foo gs inp [1 `safeDiv` x]
foo (Div:gs) inp (x:y:stack) = foo gs inp (y `safeDiv` x:stack)

foo (Pow:gs) inp []          = foo gs inp [1]
foo (Pow:gs) inp [x]         = foo gs inp [x] 
foo (Pow:gs) inp (x:y:stack) = foo gs inp (y**x:stack)

-- compile :: Genome -> (Int -> Int)
-- compile []      = error "void function"
-- compile (Inp:cs)   = \x -> x
-- compile [Lit n] = const n


safeDiv :: Double -> Double -> Double
safeDiv _ 0 = 0
safeDiv y x = y / x
easy :: Genome
easy = 
  [ Inp, Lit 2, Pow, Lit 2, Mul  
  , Inp, Lit 3, Mul              
  , Sub                          
  , Lit 10                       
  , Add                          
  ]

hard :: Genome
hard = term4 ++ term3 ++ [Sub] ++ term2 ++ [Add] ++ term1 ++ [Sub] ++ term0 ++ [Add]
  where
    term4 = [Inp, Lit 4, Pow, Lit 23, Mul] 
    term3 = [Inp, Lit 3, Pow, Lit 12, Mul] 
    term2 = [Inp, Lit 2, Pow, Lit 6,  Mul] 
    term1 = [Inp, Lit 8, Mul]              
    term0 = [Lit 37]                       

-- easy: 2x²- 3x + 10
-- hard: 23x⁴ - 12x³ + 6x² - 8x + 37
-- answer = easy 
answer :: Genome
answer = easy
answerSize = length answer


instance Gen.Gen Genome where
  done (Fen _ v) = v == 0
  type Score Genome = Down Double
  fit g = 
    let (_, v) = fitness g 
    in Fen g (Down v)

  new = do 
    n <- randomRIO (answerSize `div` 2, answerSize*2)
    ns::[Int] <- replicateM n $ randomRIO (0, 6)
    mapM toGen ns
      
  point genome = do
    let genome_len = length genome
    n::Int <- randomRIO (0, genome_len - 1)
    r::Int <- randomRIO (1, genome_len - n)

    let 
      randInsertAt g i = do
        ns <- replicateM r $ randomRIO (0, 6)
        gs'<- mapM toGen ns

        pure $ putAtIn gs' i g

        where
          putAtIn ls _ [] = ls
          putAtIn ls num rs'@(r_:rs) 
            | num == 0 = ls ++ rs'
            | otherwise = r_:putAtIn ls (num-1) rs

      randChangeAt g i = do
        ns <- replicateM r $ randomRIO (0, 6)
        gs'<- mapM toGen ns

        pure $ replaceAtIn gs' i g
        
        where
          replaceAtIn ls _ [] = ls
          replaceAtIn [] _ rs = rs
          replaceAtIn ls' num rs'@(r_:rs) 
            | num /= 0 = r_:replaceAtIn ls'(num-1) rs
            | otherwise = replaceZip ls' rs'
              where 
                replaceZip _ [] = []
                replaceZip [] rs_ = rs_
                replaceZip (l:ls) (_:rs_) = l:replaceZip ls rs
      randDeleteAt g i = pure $ delAtIn g i  
        where
          delAtIn [] _   = []
          delAtIn ls'@(l_:ls) n 
            | n == 0    = drop r ls'
            | otherwise = l_:delAtIn ls (n-1)


    a::Int <- randomRIO (0::Int, 2)
    (case a of
      0 -> randInsertAt
      1 -> randChangeAt
      _ -> randDeleteAt) genome n

  cross ls rs = do
    a :: Int <- randomRIO (0, 3)
    case a of
      0 -> pure $ altBig True ls rs
      1 -> pure $ altSmall True ls rs
      2 -> randBig ls rs
      _ -> randSmall ls rs
    where
      altBig _ [] rs' = rs'
      altBig _ ls' [] = ls' 
      altBig True  (l:ls') (_:rs') = l : altBig False ls' rs'
      altBig False (_:ls') (r:rs') = r : altBig True  ls' rs'
  
      altSmall _ [] _ = []
      altSmall _ _ [] = []
      altSmall True  (l:ls') (_:rs') = l : altSmall False ls' rs'
      altSmall False (_:ls') (r:rs') = r : altSmall True  ls' rs'
  
      randBig [] rs' = pure rs'
      randBig ls' [] = pure ls'
      randBig (l:ls') (r:rs') = do
          useLeft <- randomIO 
          rest <- randBig ls' rs'
          pure $ (if useLeft then l else r) : rest
  
      randSmall [] _ = pure []
      randSmall _ [] = pure []
      randSmall (l:ls') (r:rs') = do
          useLeft <- randomIO
          rest <- randSmall ls' rs'
          pure $ (if useLeft then l else r) : rest
  

-- (0, 6) range
toGen :: Int -> IO Gen
toGen 0 = pure Inp
toGen 1 = newLit
toGen 2 = pure Add
toGen 3 = pure Mul
toGen 4 = pure Sub 
toGen 5 = pure Div 
toGen _ = pure Pow

newLit :: IO Gen
newLit = Lit <$> randomRIO (-100, 100)

expected :: Map Int Double
expected = fromList [(i, aux i) | i :: Int <- is]
  where 
    aux :: Int -> Double
    aux i = foo answer (fromIntegral i) []
        
-- standart derivation
fitness :: Genome -> (Genome, Double)
fitness genome = adjust . sum . fmap (**2) <$> do 
  forM is $ \i -> do
    let out_i = foo genome (fromIntegral i) []
    pure (out_i - expected ! i)
  where len = length genome
        tax = fromIntegral len * 0.001
        adjust x = x + (x * tax)

-- is = [-100 .. 100]
