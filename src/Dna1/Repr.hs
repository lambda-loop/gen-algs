
module Dna1.Repr where
import qualified Data.Vector.Strict as Vec

newtype BSTH a = BSTH (Vec.Vector (Maybe a))
