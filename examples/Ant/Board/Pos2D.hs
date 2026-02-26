
module Ant.Board.Pos2D where

import Ant.Board.Compass (Compass(..))
import qualified Ant.Board.Dir as Dir

newtype Pos2D = Pos2D (Int, Int)
  deriving (Eq)

instance Show Pos2D where
  -- show (Pos2D (x, y)) = 
  --   "{x: " ++  show x ++
  --   ", " ++ "y: " ++  show y ++ "}"
  show (Pos2D (x, y)) = 
    "(" ++ show x  
    ++ "," ++ show y ++ ")"
    
distanceTo :: Pos2D -> Pos2D -> Double
distanceTo (Pos2D (x₀, y₀)) (Pos2D (x₁, y₁)) = 
  (sqrt . fromIntegral) (dx*dx + dy*dy) 
  where dx = x₀ - x₁
        dy = y₀ - y₁

-- TODO: remove all the noise
distanceToIn :: Pos2D -> (Pos2D, Compass) -> Maybe Double
distanceToIn (Pos2D (x₀, y₀)) (Pos2D (x₁, y₁), compass) = 
  case compass of
    N | y₀ < y₁ && x₀ == x₁ -> (Just . fromIntegral) (y₁-y₀) 
      | otherwise           -> Nothing

    S | y₀ > y₁ && x₀ == x₁ -> (Just . fromIntegral) (y₀-y₁) 
      | otherwise           -> Nothing
       
    E | x₀ < x₁ && y₀ == y₁ -> (Just . fromIntegral) (x₁-x₀) 
      | otherwise           -> Nothing

    W | x₀ > x₁ && y₀ == y₁ -> (Just . fromIntegral) (x₀-x₁) 
      | otherwise           -> Nothing
 
    NE | x₀ < x₁ && y₀ < y₁ -> Just whenDelta 
       | otherwise          -> Nothing

    NW | x₀ > x₁ && y₀ < y₁ -> Just whenDelta 
       | otherwise          -> Nothing

    SE | x₀ < x₁ && y₀ > y₁ -> Just whenDelta
       | otherwise          -> Nothing

    SW | x₀ > x₁ && y₀ > y₁ -> Just whenDelta
       | otherwise          -> Nothing
    where 
      whenDelta = (sqrt . fromIntegral) (dx*dx + dy*dy)
      dy = y₁ - y₀
      dx = x₁ - x₀

movedTo :: Pos2D -> Dir.Dir -> Pos2D
Pos2D (x, y) `movedTo` dir = 
  case dir of 
    Dir.Up    -> Pos2D (x, y + 1)
    Dir.Left  -> Pos2D (x - 1, y)
    Dir.Right -> Pos2D (x + 1, y) 
    Dir.Down  -> Pos2D (x, y - 1)

isIn :: Pos2D -> (Pos2D, Compass) -> Bool
isIn (Pos2D (x₀, y₀)) (Pos2D (x₁, y₁), c) = 
  case c of 
    N -> x₀ == x₁ && y₀ > y₁  
    S -> x₀ == x₁ && y₀ < y₁  
    E -> x₀ > x₁  && y₀ == y₁
    W -> x₀ < x₁  && y₀ == y₁

    NE -> x₀ > x₁ && y₀ > y₁ 
       && x₀ - x₁ == y₀ - y₁

    SW -> x₀ < x₁ && y₀ < y₁
       && x₁ - x₀ == y₁ - y₀

    NW -> x₀ < x₁ && y₀ > y₁ 
       && x₀ + y₀ == x₁ + y₁ 

    SE -> x₀ > x₁ && y₀ < y₁ 
       && x₀ + y₀ == y₀ + y₁ 
    
