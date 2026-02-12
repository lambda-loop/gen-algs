
module Data.BinTree where

data BinTree a b 
  = Branch a (BinTree a b) (BinTree a b)
  | Leaf b

-- TODO: tail rec
countStats :: BinTree a b -> (Int, Int)
countStats = go 0 0
  where
    go !b !l (Leaf _)       = (b, l + 1)
    go !b !l (Branch _ left right) = 
        let (!b1, !l1) = go (b + 1) l left
        in  go b1 l1 right
