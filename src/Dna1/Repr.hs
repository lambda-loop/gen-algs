{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}

module Dna1.Repr where

import Dna0.Fit (is) 
import Data.FlatTree  
import qualified Data.BinTree as BT
import Gen           

-- external
import qualified Data.Vector.Unboxed as UVec
import System.Random
import Data.Word (Word8)
import Data.Ord (Down(..)) 
import Control.Monad.Identity

data BinOps 
  = Add | Sub | Mul | Div | Pow 
  deriving (Show, Eq, Enum, Bounded)

-- Conversores rápidos (Inlined)
{-# INLINE toBinOp #-}
toBinOp :: Word8 -> BinOps
toBinOp = toEnum . fromIntegral

{-# INLINE fromBinOp #-}
fromBinOp :: BinOps -> Word8
fromBinOp = fromIntegral . fromEnum

{-# INLINE toLeafFn #-}
toLeafFn :: Int -> (Double -> Double)
toLeafFn (-1) = \x -> x              
toLeafFn n    = let !v = fromIntegral n in \_ -> v

newtype FastTree = FastTree { unFastTree :: FlatTree Word8 Int }
  deriving (Show, Eq, Ord)

compile :: FastTree -> (Double -> Double)
compile (FastTree t) = foldTree fBr fL t
  where
    {-# INLINE fL #-}
    fL :: Int -> (Double -> Double)
    fL val = toLeafFn val

    {-# INLINE fBr #-}
    fBr :: Word8 -> (Double -> Double) -> (Double -> Double) -> (Double -> Double)
    fBr opCode f g = case toBinOp opCode of
        Add -> \x -> f x + g x
        Sub -> \x -> f x - g x
        Mul -> \x -> f x * g x
        Div -> \x -> let d = g x in if d == 0 then 1 else f x / d 
        Pow -> \x -> let b = f x; e = g x in 
                     if isNaN b || isNaN e then 0 else b ** e

-- answer
targetFunc :: Double -> Double
-- targetFunc x = (4*(x**4)) + (13*(x**3)) - (8 * (x**2)) + (26*x) - 26
targetFunc x = (8 * (x**2)) + (26*x) - 26

dataset :: UVec.Vector (Double, Double)
dataset = UVec.fromList [ (x, targetFunc x) | i <- is, let x = fromIntegral i ]

-- instance Gen FastTree where
--     type Score FastTree = Down Double
--
--     done (Fen _ (Down score)) = score < 0.1
--
--     new = FastTree <$> genRandomFlat 4 genOp genVal
--       where
--         genOp  = fromBinOp . toEnum <$> randomRIO (0, fromEnum (maxBound :: BinOps))
--         genVal = do { r <- randomIO; if r then return (-1) else randomRIO (-10, 10) }
--
--     point (FastTree t) = FastTree <$> mutate (\d -> randomSubTree d) 4 t
--       where 
--         randomSubTree 0 = BT.Leaf <$> (do { r <- randomIO; if r then return (-1) else randomRIO (-10, 10) })
--         randomSubTree d = do
--             isBr <- randomIO
--             if isBr 
--             then BT.Branch <$> (fromBinOp . toEnum <$> randomRIO (0, fromEnum (maxBound :: BinOps)))
--                            <*> randomSubTree (d-1) <*> randomSubTree (d-1)
--             else BT.Leaf   <$> (do { r <- randomIO; if r then return (-1) else randomRIO (-10, 10) })
--
--     cross (FastTree a) (FastTree b) = (FastTree . fst) <$> crossover a b
--
--     fit tree = 
--         let !f = compile tree
--             
--             totalSqError = UVec.foldl' (\acc (inp, expected) -> 
--                     let !got = f inp
--                         !diff = got - expected
--                         !sqErr = if isNaN diff || isInfinite diff 
--                                  then 1_000_000.0 
--                                  else diff * diff
--                     in acc + sqErr
--                     ) 0.0 dataset
--             
--             mse = totalSqError / fromIntegral (UVec.length dataset)
--         in Fen tree (Down mse)
