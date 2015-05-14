-- http://www.reddit.com/r/dailyprogrammer/comments/32o5je/20150415_challenge_210_intermediate_drawing_a/
-- Draw a gradient from scratch

data Pixel = Pixel {
                     red    ::   Int,
                     green  ::   Int,
                     blue   ::   Int
                   } deriving (Show)


-- Generate a horizontal gradient from color A to B of arbitrary length
-- Find slopes for each line
-- Probably a better way to do this but then what's the fun of the challenge?
horizontalGradient :: Int -> Pixel -> Pixel -> [Pixel]
horizontalGradient length colorA colorB =
    let redGradient = lineIntPos (red colorA) (red colorB) length
        blueGradient = lineIntPos (blue colorA) (blue colorB) length
        greenGradient = lineIntPos (green colorA) (green colorB) length
        posNumbers = [0..length-1]
        redMap = map redGradient posNumbers
        blueMap = map blueGradient posNumbers
        greenMap = map greenGradient posNumbers
    in map tupleToPixel $ zip3 redMap greenMap blueMap

tupleToPixel :: (Int, Int, Int) -> Pixel
tupleToPixel (r,g,b) = Pixel r g b

-- (0,a) -> (length, b)
-- y = mx + a
-- Length is zero indexed, last digit will be length - 1 before out-of-bounds
lineIntPos :: (Integral a) => a -> a -> a -> a -> a
lineIntPos start end length pos =
    let num = end - start
        denom = length - 1
    in round ((fromIntegral (num * pos)) / (fromIntegral denom)) + start


------------------------------------------------------
-- Code for netpbm -> PPM

ppmHeader :: Int -> Int -> String
ppmHeader width height = "P3\n" ++ show width ++ " " ++ show height ++ " 255\n"

pixelToPPM :: Pixel -> String
pixelToPPM (Pixel r g b) = show r ++ " " ++ show g ++ " " ++ show b

pixelLineToPPM :: [Pixel] -> String
pixelLineToPPM pixels = unwords (map pixelToPPM pixels)

-- Squares, why!
pixelSquareToPPM :: [[Pixel]] -> String
pixelSquareToPPM pixelest = unlines (map pixelLineToPPM pixelest)


writePPM :: FilePath -> String -> IO ()
writePPM path ppmString = writeFile path ppmString


main = do
    let colorA = Pixel 204 119 34
    let colorB = Pixel 1 66 37
    let length = 1000
    let height = 100
    let pixels = horizontalGradient length colorA colorB
    let pixelBox = replicate height pixels
    let ppmContents = (ppmHeader length height) ++ (pixelSquareToPPM pixelBox)
    writePPM "image.ppm" ppmContents
