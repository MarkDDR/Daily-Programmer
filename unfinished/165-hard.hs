-- http://www.reddit.com/r/dailyprogrammer/comments/27h53e/662014_challenge_165_hard_simulated_ecology_the/
-- Simulated Forest Ecology

import qualified Data.Vector as V

data Tree       = Sapling Int | Tree Int | Elder
data Entity     = Lumberjack | Bear | Tree

type YearLumber = Int
type YearMaw    = Bool
type Month      = Int
type Position   = (Int, Int) -- Row, Column
type EntityPos  = (Entity, Position)


