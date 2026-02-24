

module Ant.Board.Compass where
import qualified Ant.Board.Dir as Dir

data Compass
  = N  | S  | E  | W
  | NE | NW | SE | SW
  deriving (Show, Eq, Ord)

adjustedTo :: Compass -> Dir.Dir -> Compass
c  `adjustedTo` Dir.Up    = c
N  `adjustedTo` Dir.Down  = S
S  `adjustedTo` Dir.Down  = N
E  `adjustedTo` Dir.Down  = W
W  `adjustedTo` Dir.Down  = E
NE `adjustedTo` Dir.Down  = SW
NW `adjustedTo` Dir.Down  = SE
SE `adjustedTo` Dir.Down  = NW
SW `adjustedTo` Dir.Down  = NE

N  `adjustedTo` Dir.Left  = W
S  `adjustedTo` Dir.Left  = E
E  `adjustedTo` Dir.Left  = N
W  `adjustedTo` Dir.Left  = S
NE `adjustedTo` Dir.Left  = N
NW `adjustedTo` Dir.Left  = SW
SE `adjustedTo` Dir.Left  = NE
SW `adjustedTo` Dir.Left  = SE

N  `adjustedTo` Dir.Right = E
S  `adjustedTo` Dir.Right = W
E  `adjustedTo` Dir.Right = S
W  `adjustedTo` Dir.Right = N
NE `adjustedTo` Dir.Right = SE
NW `adjustedTo` Dir.Right = NE
SE `adjustedTo` Dir.Right = SW
SW `adjustedTo` Dir.Right = NW

