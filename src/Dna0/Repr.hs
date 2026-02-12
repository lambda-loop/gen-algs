
module Dna0.Repr where

import System.Random

data Expr 
  = Inp 
  | Lit Int
  | Neg Expr
  | Inv Expr
  | Add Expr Expr
  | Mul Expr Expr
  | Pow Expr Expr
  deriving (Eq, Show, Ord)

e1 = Add (Neg Inp) (Pow Inp (Mul (Lit 3) Inp))
e2 = Mul (Lit 3) (Pow Inp (Lit 2))
e3 = Add (Add (Pow Inp (Lit 2)) (Mul (Lit 3) Inp)) (Neg (Lit 13))

size :: Expr -> Int
size Inp         = 0
size (Lit _)     = 0
size (Neg e)     = 1 + size e
size (Inv e)     = 1 + size e
size (Add el er) = 1 + max (size el) (size er)
size (Mul el er) = 1 + max (size el) (size er)
size (Pow el er) = 1 + max (size el) (size er)

randomDnaWithDepth :: Int -> IO Expr
randomDnaWithDepth 0 = do 
  b <- randomIO
  if b then pure Inp

  else do
    x <- newLit
    pure (Lit x)
 
 -- n > 0, please
randomDnaWithDepth max_depth = do
  n::Int <- randomRIO (1, 7)
  case n of
    1 -> pure Inp
    2 -> Lit <$> newLit
    3 -> Add <$> randomDnaWithDepth (max_depth-1) <*> randomDnaWithDepth (max_depth-1)
    4 -> Neg <$> randomDnaWithDepth (max_depth-1)
    5 -> Mul <$> randomDnaWithDepth (max_depth-1) <*> randomDnaWithDepth (max_depth-1)
    6 -> Inv <$> randomDnaWithDepth (max_depth-1)
    7 -> Pow <$> randomDnaWithDepth (max_depth-1) <*> randomDnaWithDepth (max_depth-1)
    _ -> error "reach to an impossible cenario"
 
-- how to generalize it? its kinda a partial fmap in the ExprF
randCut :: Expr -> IO (Expr, Expr -> Expr)
randCut (Neg e) = pure (e, Neg)
randCut (Inv e) = pure (e, Inv)

randCut (Add el er) = do 
  goLeft <- randomIO
  pure $ if goLeft 
    then (el, (`Add` er)) 
    else (er, (el `Add`))

randCut (Mul el er) = do 
  goLeft <- randomIO
  pure $ if goLeft 
    then (el, (`Mul` er)) 
    else (er, (el `Mul`))

randCut (Pow el er) = do 
  goLeft <- randomIO
  pure $ if goLeft 
    then (el, (`Pow` er)) 
    else (er, (el `Pow`))

randCut other = pure (other, id)

newLit :: IO Int
-- newLit = randomIO 
newLit = randomRIO defaultRange
defaultRange :: (Int, Int)
defaultRange = (-100,100)
