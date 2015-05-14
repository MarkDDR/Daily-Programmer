-- http://www.reddit.com/r/dailyprogrammer/comments/2ug3hx/20150202_challenge_200_easy_floodfill/
-- Flood Fill

import qualified Data.Vector as V
import Data.Maybe

type Point = (Int, Int) -- Row, Column
type VecVec a = V.Vector (V.Vector a)
type VisitPaint Bool = Maybe Bool -- Nothing means not visited, Just means paint (true) or no paint (false)

pointAt :: Point -> VecVec a -> a
pointAt (r,c) board = (board ! r) ! c


floodFill :: Char -> Point -> VecVec Char -> VecVec Char
floodFill replaceChar start board = 
    let consumeChar = pointAt start board
        blankBoard = V.replicate (length board) (V.replicate (length  . length board) No)
        visitPaintBoard = floodFillAct replaceChar consumeChar start board blankBoard
    in  paint replaceChar board visitPaintBoard

floodFillAct :: Char -> Point -> VecVec Char -> VecVec VisitPaint -> VecVec VisitPaint
floodFillAct consumeChar (row,column) board visited
    | isJust $ pointAt (row,column) visited     = visited
    | pointAt (row,column) board /= consumeChar = visited // [(row, noPaintColumn)]
    | otherwise                                 = visitedD
    where
        noPaintColumn   = (visited ! row) // [(column, Just False)]
        yesPaintColumn  = (visited ! row) // [(column, Just True)]
        visitedA        = if (row - 1) < 0 then yesPaintColumn
                            else floodFillAct consumeChar (row - 1, column) board yesPaintColumn
        visitedB        = if (row + 1) >= (V.length board) then visitedA
                            else floodFillAct consumeChar (row + 1, column) board visitedA
        visitedC        = if (column - 1) < 0 then visitedB
                            else floodFillAct consumeChar (row, column - 1) board visitedB
        visitedD        = if (column + 1) >= (V.length (board ! 0)) then visitedC
                            else floodFillAct consumeChar (row, column + 1) board visitedC

paint :: a -> VecVec a -> VecVec VisitPaint -> Point -> VecVec a
paint color wall places (r,c)
    | isNothing $ pointAt (r,c) places   = nextColumn wall
    | (not . fromJust) (pointAt (r,c) places) = nextColumn wall
    | otherwise =   let newWall = wall // (r, ((wall ! r) // (c, color)))
                    in nextColumn newWall
    where
        nextRow wall' = if (r + 1) >= (V.length places)
                            then wall'
                            else paint color wall' places (r + 1, 0)
        nextColumn wall' = if (c + 1) >= (V.length (places ! 0))
                            then nextRow wall'
                            else paint color wall' places (r, c + 1)

printVecVec :: VecVec Char IO -> IO ()
printVecVec board = do putStrLn . V.toList . V.head $ board
                    if null . V.tail board
                        then return
                        else printVecVec (V.tail board)

