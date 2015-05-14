-- http://www.reddit.com/r/dailyprogrammer/comments/351b0o/20150506_challenge_213_intermediate_the_lazy/
-- Lazy Typist

import qualified Data.Vector as V
import Data.Maybe

type VecVec = Vector (Vector Char)
type Coord = (Int, Int) -- Row, Column
type Effort = Int


keyboard :: VecVec
keyboard = V.Map V.fromList . V.fromList   ["qwertyuiop"
                                            "asdfghjkl"
                                            "^zxcvbnm^"
                                            "   #####"]



-- Manhattan Distance between two points
manDistance :: Coord -> Coord -> Effort
manDistance (x1, y1) (x2, y2) = x3 + y3
    where
        x3 = abs (x1 - x2)
        y3 = abs (y1 - y2)

-- Find lowercase character coordinate
findChar :: Int -> Char -> Coord
findChar row chr =
    let curRow = keyboard ! row
        charIndex = V.findIndex (== chr) curRow
    in if isJust charIndex
        then (row, fromJust charIndex)
        else findChar (row+1) chr

-- Find closest spacebar character (assume last row has spacebar)
closestSpaceCoord :: Coord -> Coord
closestSpaceCoord fingerCoord = 
    let spaceCoords = [(3,3), (3,4), (3,5), (3,6), (3,7)]
        efforts = map (manDistance fingerCoord) spaceCoords
    in minimum efforts

