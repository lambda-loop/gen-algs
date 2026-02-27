
module Main where
import Polynomial.Expr
import qualified Strategy.IslandModel as IM
import qualified Strategy.HallOfFame as HF
import Ant.World.Screen (runTest)
import Ant.Dna
import Ant.Instance

main :: IO ()
main = do
  -- a :: Expr <- IM.evolve 5 20 2
  a :: Mind <- IM.evolve 4 10 1
  -- a :: Expr <- HF.evolve 
  print a
  runTest
  
