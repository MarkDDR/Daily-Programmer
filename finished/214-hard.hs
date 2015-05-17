-- http://www.reddit.com/r/dailyprogrammer/comments/3629st/20150515_challenge_214_hard_chester_the_greedy/
-- Total distance to collect all treats using nearest-first algorithm

import System.Environment
-- Data.Vector for more efficient lists
import qualified Data.Vector.Unboxed as V

type Point = (Double, Double)
type Points = V.Vector Point
type Distance = Double

distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt $ (x1 - x2)^2 + (y1 - y2)^2

-- Simple Brute Force Algorithm (There is probably a better method though)
-- A more efficient algorithm would probably order the list in a better way
totalDistanceBrute :: Distance -> Point -> Points -> Distance
totalDistanceBrute travelled location pointList =
    let distanceList = V.map (distance location) pointList
        minDistIndex = V.minIndex distanceList
        minDistValue = distanceList V.! minDistIndex
        minDistPoint = pointList V.! minDistIndex
        -- change head and min value so I can just get tail
        newPointList = pointList V.// [(minDistIndex, V.head pointList), (0, minDistPoint)]
        newTravelled = travelled + minDistValue
        -- Hey, they all have the same name length
        -- That's actually slightly confusing to look at

        -- This if statement is here because I don't know a way to have check if a vector is empty
        -- with an overloaded function like you can with lists (E.G. f a [] = a)
    in  if (V.null $ V.tail newPointList)
            then newTravelled
            else totalDistanceBrute newTravelled minDistPoint $ V.tail newPointList

pair :: [a] -> [(a,a)]
pair [] = []
pair (x:[]) = error "Something terrible has happened with your input" -- This shouldn't ever happen
-- If I were to actual generalize this then this would be `pair (x:[]) = []`
pair (x:y:xs) = ((x,y): pair xs)

main = do
    args <- getArgs
    progName <- getProgName
    file <- readFile $ head args
    if null args
        then error $ "USAGE: " ++ progName ++ " pointListFile"
             -- This then doesn't every happen because the `file <-` before is apparently not lazy
        else let    pointList = pair . map read . tail . words $ file
                    pointVec = V.fromList pointList
                    totDist = totalDistanceBrute 0 (0.5, 0.5) pointVec
             in     print totDist
