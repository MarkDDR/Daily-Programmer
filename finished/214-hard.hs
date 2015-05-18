-- http://www.reddit.com/r/dailyprogrammer/comments/3629st/20150515_challenge_214_hard_chester_the_greedy/
-- Total distance to collect all treats using nearest-first algorithm

import System.Environment
-- Data.Vector for more efficient lists
import qualified Data.Vector.Unboxed as V

type Point = (Double, Double)
type Points = V.Vector Point
type Distance = Double

-- Optimization to use less sqrt's
distanceSquared :: Point -> Point -> Double
distanceSquared (x1, y1) (x2, y2) = (x1 - x2)^2 + (y1 - y2)^2

-- Not really used
distance :: Point -> Point -> Double
distance pt1 pt2 = sqrt $ distanceSquared pt1 pt2

-- Simple Brute Force Algorithm (There is probably a better method though)
-- A more efficient algorithm would probably order the list in a better way
totalDistanceBrute :: Distance -> Point -> Points -> Distance
totalDistanceBrute travelled location pointList =
    let distanceList = V.map (distanceSquared location) pointList
        minDistIndex = V.minIndex distanceList
        minDistValue = sqrt $ distanceList V.! minDistIndex -- sqrt to calculate true distance
        minDistPoint = pointList V.! minDistIndex
        -- swap head and min value so I can remove by calling tail
        newPointList = pointList V.// [(minDistIndex, V.head pointList), (0, minDistPoint)]
        newTravelled = travelled + minDistValue
        -- Hey, they all have the same name length
        -- That's actually slightly confusing to look at

        -- This if statement is here because I don't know a way to pattern match an empty vector
        -- like you can with lists (E.G. f a [] = a)
    in  if (V.null $ V.tail newPointList)
            then newTravelled
            else totalDistanceBrute newTravelled minDistPoint $ V.tail newPointList

pair :: [a] -> [(a,a)]
pair [] = []
pair (x:[]) = error "Something terrible has happened with your input" -- This shouldn't ever happen
-- If I were to actually generalize this then this would be `pair (x:[]) = []`
pair (x:y:xs) = ((x,y): pair xs)

-- Input should be a text file as described in the challenge text

main = do
    args <- getArgs
    progName <- getProgName
    file <- readFile $ head args
    if null args
        then error $ "USAGE: " ++ progName ++ " pointListFile.txt"
             -- This then doesn't every happen because the `file <- ...` before is apparently not lazy
             -- and `head args` errors. Oh well, won't fix
        else let    pointList = pair . map read . tail . words $ file
                    -- tail is used to get rid of the number at the beginning of input, as it's not used
                    pointVec = V.fromList pointList
                    totDist = totalDistanceBrute 0 (0.5, 0.5) pointVec
             in     print totDist
