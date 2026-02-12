{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.FlatTree where

import qualified Data.BinTree as BT
import qualified Data.Vector.Unboxed as UVec
import qualified Data.Vector.Unboxed.Mutable as MUVec
import qualified Data.Bit as Bit
import Data.Vector.Unboxed ((!))
import Control.Monad.ST
import Control.Monad (forM_, when)
import System.Random (randomIO, randomRIO)
import Data.Word (Word8)

data FlatTree a b = FlatTree
    { _tags     :: !(UVec.Vector Bit.Bit)
    , _branches :: !(UVec.Vector a)
    , _leaves   :: !(UVec.Vector b)
    , _sizes    :: !(UVec.Vector Int)
    } deriving (Show, Eq) 

data Cursor a b = Cursor
    { _idx  :: !Int
    , _tree :: !(FlatTree a b)
    }

data View a b 
    = VBranch a (Cursor a b) (Cursor a b)
    | VLeaf b
    | VInvalid

getRank :: UVec.Vector Bit.Bit -> Int -> Bool -> Int
getRank tags i True  = Bit.countBits (UVec.take i tags)
getRank tags i False = i - Bit.countBits (UVec.take i tags)
{-# INLINE getRank #-}

pack :: (UVec.Unbox a, UVec.Unbox b) => BT.BinTree a b -> FlatTree a b
pack tree = runST $ do
    let (!nBranches, !nLeaves) = BT.countStats tree
        !totalNodes = nBranches + nLeaves

    mTags   <- MUVec.unsafeNew totalNodes
    mSizes  <- MUVec.unsafeNew totalNodes
    mBrs    <- MUVec.unsafeNew nBranches 
    mLvs    <- MUVec.unsafeNew nLeaves    

    let go !i !ib !il (BT.Leaf v) = do
            MUVec.unsafeWrite mTags  i (Bit.Bit False)
            MUVec.unsafeWrite mSizes i 1
            MUVec.unsafeWrite mLvs   il v
            return (i + 1, ib, il + 1, 1)

        go !i !ib !il (BT.Branch v l r) = do
            MUVec.unsafeWrite mTags i (Bit.Bit True)
            MUVec.unsafeWrite mBrs  ib v
            
            (!i2, !ib2, !il2, !szL) <- go (i + 1) (ib + 1) il l
            
            (!i3, !ib3, !il3, !szR) <- go i2 ib2 il2 r
            
            let !my_size = 1 + szL + szR
            MUVec.unsafeWrite mSizes i my_size
            
            return (i3, ib3, il3, my_size)

    _ <- go 0 0 0 tree

    FlatTree 
        <$> UVec.unsafeFreeze mTags
        <*> UVec.unsafeFreeze mBrs
        <*> UVec.unsafeFreeze mLvs
        <*> UVec.unsafeFreeze mSizes

root :: FlatTree a b -> Cursor a b
root = Cursor 0 

{-# INLINE view #-}
view :: (UVec.Unbox a, UVec.Unbox b) => Cursor a b -> View a b
view (Cursor i t)
    | i >= UVec.length (_tags t) = VInvalid
    | otherwise = 
        case (_tags t) ! i of
            Bit.Bit True -> 
                let !dataIdx = getRank (_tags t) i True
                    !val     = (_branches t) ! dataIdx
                    
                    !leftIdx  = i + 1
                    !leftSize = (_sizes t) ! leftIdx
                    !rightIdx = leftIdx + leftSize
                    
                    !left     = Cursor leftIdx t
                    !right    = Cursor rightIdx t 
                in VBranch val left right

            Bit.Bit False ->
                let !dataIdx = getRank (_tags t) i False
                    !val     = (_leaves t) ! dataIdx
                in VLeaf val

{-# INLINE foldTree #-}
foldTree :: (UVec.Unbox a, UVec.Unbox b) 
         => (a -> c -> c -> c) -- ^ Branch function
         -> (b -> c)           -- ^ Leaf function
         -> FlatTree a b 
         -> c
foldTree fBr fL t = go 0
  where
    tags = _tags t
    brs  = _branches t
    lvs  = _leaves t
    sizes = _sizes t

    go !i = 
        case tags ! i of
            Bit.Bit False -> 
                let !di = getRank tags i False
                in fL (lvs ! di)
            
            Bit.Bit True ->
                let !di = getRank tags i True
                    !val = brs ! di
                    
                    !idxL = i + 1
                    !szL  = sizes ! idxL
                    !idxR = idxL + szL
                    
                    !resL = go idxL
                    !resR = go idxR
                in fBr val resL resR

data Selection = Sel 
    { selIdx :: !Int 
    , selSize :: !Int
    , selAncestors :: ![Int] 
    } deriving Show

uniformSelect :: FlatTree a b -> IO Selection
uniformSelect t = go 0 []
  where
    szs = _sizes t
    tags = _tags t

    go !i !ancestors = do
        let !size = szs ! i
        
        stop <- (== 0) <$> randomRIO (0, size - 1 :: Int)
        
        if stop || size == 1 
        then return $ Sel i size ancestors
        else do
            let !idxL = i + 1
                !szL  = szs ! idxL
                !hasRight = size > (1 + szL)
            
            if not hasRight
            then go idxL (i : ancestors)
            else do
                r <- randomRIO (0, (size - 1) - 1)
                if r < szL 
                then go idxL (i : ancestors)
                else do
                    let !idxR = idxL + szL
                    go idxR (i : ancestors)

crossover :: (UVec.Unbox a, UVec.Unbox b) 
          => FlatTree a b 
          -> FlatTree a b 
          -> IO (FlatTree a b, FlatTree a b)
crossover t1 t2 = do
    sel1 <- uniformSelect t1
    sel2 <- uniformSelect t2
    
    let child1 = graft t1 sel1 t2 sel2
        child2 = graft t2 sel2 t1 sel1
        
    return (child1, child2)

mutate :: (UVec.Unbox a, UVec.Unbox b)
       => (Int -> IO (BT.BinTree a b)) 
       -> Int                          
       -> FlatTree a b 
       -> IO (FlatTree a b)
mutate genFn maxDepth t = do
    sel <- uniformSelect t
    
    let estimatedDepth = min 3 maxDepth 
    newBinTree <- genFn estimatedDepth
    let newFlat = pack newBinTree
        
        selNew = Sel 0 ((_sizes newFlat) ! 0) [] 
        
    return $ graft t sel newFlat selNew

graft :: (UVec.Unbox a, UVec.Unbox b) 
      => FlatTree a b -> Selection 
      -> FlatTree a b -> Selection 
      -> FlatTree a b
graft host (Sel hIdx hSz hAncs) donor (Sel dIdx dSz _) = 
    let
        delta = dSz - hSz
        
        preTags = UVec.take hIdx (_tags host)
        sufTags = UVec.drop (hIdx + hSz) (_tags host)
        
        brBefore = getRank (_tags host) hIdx True
        lvBefore = getRank (_tags host) hIdx False
        
        endIdx   = hIdx + hSz
        brTotalHost = UVec.length (_branches host)
        lvTotalHost = UVec.length (_leaves host)
        brInOld = getRank (_tags host) endIdx True - brBefore
        lvInOld = getRank (_tags host) endIdx False - lvBefore
        
        preBrs = UVec.take brBefore (_branches host)
        sufBrs = UVec.drop (brBefore + brInOld) (_branches host)
        
        preLvs = UVec.take lvBefore (_leaves host)
        sufLvs = UVec.drop (lvBefore + lvInOld) (_leaves host)
        
        preSzs = UVec.take hIdx (_sizes host)
        sufSzs = UVec.drop endIdx (_sizes host)

        subTags = UVec.slice dIdx dSz (_tags donor)
        
        dBrStart = getRank (_tags donor) dIdx True
        dBrEnd   = getRank (_tags donor) (dIdx + dSz) True
        subBrs   = UVec.slice dBrStart (dBrEnd - dBrStart) (_branches donor)
        
        dLvStart = getRank (_tags donor) dIdx False
        dLvEnd   = getRank (_tags donor) (dIdx + dSz) False
        subLvs   = UVec.slice dLvStart (dLvEnd - dLvStart) (_leaves donor)
        
        subSzs   = UVec.slice dIdx dSz (_sizes donor)

        newTags = UVec.concat [preTags, subTags, sufTags]
        newBrs  = UVec.concat [preBrs,  subBrs,  sufBrs]
        newLvs  = UVec.concat [preLvs,  subLvs,  sufLvs]
        
        baseSizes = UVec.concat [preSzs, subSzs, sufSzs]
        
        finalSizes = UVec.modify (\v -> 
            forM_ hAncs $ \ancestorIdx -> do
                oldS <- MUVec.unsafeRead v ancestorIdx
                MUVec.unsafeWrite v ancestorIdx (oldS + delta)
            ) baseSizes

    in FlatTree newTags newBrs newLvs finalSizes

genRandomFlat :: (UVec.Unbox a, UVec.Unbox b) 
              => Int 
              -> IO a 
              -> IO b 
              -> IO (FlatTree a b)
genRandomFlat depth genA genB = do
    tree <- genBinTree depth
    return $ pack tree
  where
    genBinTree 0 = BT.Leaf <$> genB
    genBinTree d = do
        isBranch <- randomIO :: IO Bool
        if isBranch
        then BT.Branch <$> genA <*> genBinTree (d-1) <*> genBinTree (d-1)
        else BT.Leaf   <$> genB
