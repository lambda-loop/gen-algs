
module Dna0.Mut where

-- external 
import System.Random

-- internal
import Dna0.Repr

-- reproductions 
pointMut :: Expr -> IO Expr
pointMut dna =
  let dna_len = size dna - 1
  in do
    n :: Int <- randomRIO (0, dna_len)
    mutIn n dna
  where 
    mutIn n expr =
      case expr of
        Inp -> randomDnaWithDepth 3
        Lit _ -> Lit <$> newLit
        _ | n <= 0 -> randomDnaWithDepth 3
          | otherwise -> do
            (inner, iter) <- randCut expr
            iter <$> mutIn (n-1) inner

subTreeMut :: Expr -> IO Expr
subTreeMut expr = do 
  depth <- randomRIO (0, size expr)
  expr `withNewSubIn` depth

  where 
    e `withNewSubIn` 0 = do
      randomDnaWithDepth (size e)

    e `withNewSubIn` n = do
      (inner, iter) <- randCut e
      iter <$> inner `withNewSubIn` (n-1)

-- Crossover 
crossOverWith :: Expr -> Expr -> IO (Expr, Expr)
crossOverWith el er = do
  let nl = size el
  let nr = size er

  randSwapInDepth (el, er) (nl, nr)

randSwapInDepth :: (Expr, Expr) -> (Int, Int) -> IO (Expr, Expr)
randSwapInDepth (el, er) (nl, nr)
  | stopPoint = pure (er, el)
  | otherwise = do
    (inner_l, iter_l) <- do
      if nl == 0 then pure (el, id)
      else randCut el

    (inner_r, iter_r) <-
      if nr == 0 then pure (er, id)
      else randCut er

    (rec_l, rec_r) <- randSwapInDepth (inner_l, inner_r) (nl-1, nr-1)
    pure (iter_l rec_l, iter_r rec_r)

  where
    stop_l = isNulary el || nl == 0
    stop_r = isNulary er || nr == 0
    stopPoint = stop_l && stop_r

    isNulary Inp     = True
    isNulary (Lit _) = True
    isNulary _       = False



