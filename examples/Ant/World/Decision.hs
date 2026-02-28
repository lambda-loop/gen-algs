{-# LANGUAGE RecordWildCards #-}

module Ant.World.Decision where
import Ant.Dna
import qualified Ant.World.State as World
import qualified Data.Vector.Strict as Vec
import Control.Monad.Identity
import Control.Monad
import Control.Monad.State hiding (state)
import Ant.Scanner

import qualified Data.Vector.Unboxed.Mutable as MUVec
import Control.Monad.ST
import Data.STRef


data Move 
  = TurnLeft
  | TurnRight
  | KeepGoing
  deriving (Eq, Show, Ord)

-- decide' :: DecisionTree -> World.State -> Bool
-- decide' tree state = evalState (decide tree state) []

decideST :: DecisionTree -> World.State -> Bool
decideST tree state = runST $ do
  -- Tamanho máximo da pilha: profundidade da árvore (ou um valor fixo seguro)
  let maxDepth = length tree 
  stack <- MUVec.new maxDepth
  ptr   <- newSTRef 0  -- ponteiro para o topo

  -- Função auxiliar para push
  let push !b = do
        p <- readSTRef ptr
        MUVec.write stack p b
        writeSTRef ptr (p + 1)

  -- Função para pop (retorna o topo e decrementa)
  let pop = do
        p <- readSTRef ptr
        let p' = p - 1
        writeSTRef ptr p'
        MUVec.read stack p'

  -- Função para peek (ver topo sem remover)
  let peek = do
        p <- readSTRef ptr
        MUVec.read stack (p - 1)

  -- Processa cada gene
  forM_ tree $ \g -> case g of
    Left And -> do
      p <- readSTRef ptr
      case p of
        0 -> push False
        1 -> do
          b <- peek
          push b  
        _ -> do
          br <- pop
          bl <- pop
          push (bl && br)
    Left Or -> do
      p <- readSTRef ptr
      case p of
        0 -> push True   
        1 -> do
          b <- peek
          push b
        _ -> do
          br <- pop
          bl <- pop
          push (bl || br)
    Left Not -> do
      p <- readSTRef ptr
      if p == 0
        then pure ()   
        else do
          b <- pop
          push (not b)
    Right scanner -> do
      let ant = (World.ant_pos . World.player) state
          vec = case target scanner of
            Food  -> World.foods state
            Block -> World.blocks state
      let b = scan ant vec (compass scanner) (comparator scanner) (comparated scanner)
      push b

  -- Resultado final
  p <- readSTRef ptr
  if p == 0 then pure False else peek


decide :: DecisionTree -> World.State -> State [Bool] Bool
decide tree state = do
  Vec.forM_ tree $ \g -> do
    modify (step g)

  stack <- get
  pure $ case stack of 
          []    -> False 
          (b:_) -> b
  where
    step :: Gene -> [Bool] -> [Bool]
    step (Left And) []  = [False]
    step (Left And) [b] = [b]
    step (Left And) (bl:br:stack) = (bl && br) : stack

    step (Left Or) (bl:br:stack) = (bl || br) : stack
    step (Left Or) _ = [True]
      
    step (Left Not) [] = []
    step (Left Not) (b:stack) = not b : stack

    step (Right Scanner {..}) stack = 
      scan ant vec compass comparator comparated : stack       
      where 
        ant = (World.ant_pos . World.player) state
        vec = case target of
          Food  -> World.foods state
          Block -> World.blocks state
 
-- TODO: Scanners shouldent repeat themselfs..
-- WARNING: Slow implementation
act :: Mind -> World.State -> Move
act Mind {..} state
  | decideST shouldKeepGoing state = KeepGoing
  | decideST shouldTurnLeft state  = TurnLeft
  | otherwise                     = TurnRight


-- decide :: DecisionTree -> World.State -> State [Bool] Bool
