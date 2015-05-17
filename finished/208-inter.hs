-- http://www.reddit.com/r/dailyprogrammer/comments/3104wu/20150401_challenge_208_intermediate_ascii/
-- ASCII Gradients

type Point = (Int, Int)
type DPoint = (Double, Double)
type Slope = Double
type YIntercept = Double
type Line = (Slope, YIntercept)

distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt (dx^2 + dy^2)
    where
        dx = fromIntegral (x1 - x2)
        dy = fromIntegral (y1 - y2)


distanceToCharGrad :: Int -> [Char] -> Double -> Char
distanceToCharGrad r charList d
    | floor d >= r    = last charList
    | floor d == 0    = head charList
    | otherwise = charList !! floor ((d * (fromIntegral $ length charList)) / fromIntegral r)


genRadial :: (Int, Int, Int) -> (Int, Int) -> [Char] -> [String]
genRadial _ (_,0) _ = [""]
genRadial (x,y,r) (l,h) charList = 
    let distanceArgChange x' = distance (x,y) (x',h)
        xIndexToChar = distanceToCharGrad r charList . distanceArgChange
        radialRow = map xIndexToChar [0..l]
    in radialRow : genRadial (x,y,r) (l,h-1) charList


-- Returns where in a line a point would intersect if a perpendicular line was drawn
-- This is explained better if you draw a picture of it
linePointIntersect :: Point -> Line -> DPoint
linePointIntersect (x,y) (m, b) =
    -- You can work this formula out yourself if you solve a system of equations for
    -- a line (m,b) and the perpendicular line that passes through the point (x,y)
    let n = negate . recip $ m
        c = (fromIntegral y) - (n * (fromIntegral x))
        x2 = (-c + b) / (n - m)
        y2 = (m * x2) + b
    in  (x2, y2)

-- TODO: Finish genLinear
-- genLinear :: Point -> Point -> [Char] -> 

main = do
    let charList = "aaabcccdeeefggg"
    let xyr = ((-10), 20, 60)
    let size = (40, 40)
    let grad = genRadial xyr size charList
    (putStr . unlines) grad
