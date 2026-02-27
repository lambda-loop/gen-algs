
module Main where
import Polynomial.Expr
import qualified Strategy.IslandModel as IM
import qualified Strategy.HallOfFame as HF
import Ant.World.Screen (runTest, celebrate)
import Ant.Dna
import Ant.Instance

main :: IO ()
main = do
  -- a :: Expr <- IM.evolve 5 20 2
  a :: Mind <- IM.evolve 4 100 1
  -- a :: Expr <- HF.evolve 
  print a
  print "lets celebrate:"
  celebrate a
  -- runTest
  
