-- http://www.reddit.com/r/dailyprogrammer/comments/36m83a/20150520_challenge_215_intermediate_validating/
-- Sorting network verification

-- Vectors for quickly accessing arbitrary indices
import qualified Data.Vector.Unboxed as V
import Data.List (sort) 

-- Sorting Network is represented as a series of comparators
type SortNetwork = [Comparator]
-- Compare index A to index B
-- Index A should always be a smaller num than index B
type Comparator = (Int, Int)
-- We are only comparing 1 and 0 to find out if a sorting network is valid
type Input = V.Vector Bool
type Output = V.Vector Bool
type Wires = Int

-- Simulate a single comparator
doComparator :: Input -> Comparator -> Input
doComparator input (a, b)
    | inB < inA     = input V.// [(a, inB), (b, inA)]
    | otherwise     = input
    where
        inA = input V.! a
        inB = input V.! b

-- Simulate a sorting network
doNetwork :: SortNetwork -> Input -> Output
doNetwork [] input = input
doNetwork (c:cs) input = doNetwork cs $ doComparator input c

-- Taken from http://snipplr.com/view/11807/
decToBin x = reverse $ decToBin' x
    where
        decToBin' 0 = []
        decToBin' y = let (a,b) = quotRem y 2 in [b] ++ decToBin' a

intToBinaryBool :: Int -> [Bool]
intToBinaryBool num = map (\i -> if i == 1 then True else False) $ decToBin num

genInputs :: Wires -> [Input]
genInputs wiresInNetwork = map (pad wiresInNetwork . V.fromList . intToBinaryBool) [0 .. 2^wiresInNetwork - 1]

genOutputs :: Wires -> [Output]
genOutputs wiresInNetwork = map (pad wiresInNetwork . V.fromList . sort . intToBinaryBool) [0 .. 2^wiresInNetwork - 1]

-- Add False to the beginning if vector doesn't start with it
pad :: Wires -> V.Vector Bool -> V.Vector Bool
pad wires input = if inputLength < wires
                    then V.replicate (wires - inputLength) False V.++ input
                    else input
                  where inputLength = V.length input


-- True means works, false means doesn't work
checkNetwork :: Wires -> SortNetwork -> Bool
checkNetwork wires network = checkNetwork' network (inputs, outputs)
    where
        inputs = genInputs wires
        outputs = genOutputs wires
        checkNetwork' _ ([],[]) = True
        checkNetwork' net (i:is, o:os) = if doNetwork net i == o
                                            then checkNetwork' net (is,os)
                                            else False

pair :: [a] -> [(a,a)]
pair [] = []
pair (x:[]) = error "Something terrible has happened with your input" -- This shouldn't ever happen
-- If I were to actually generalize this then this would be `pair (x:[]) = []`
pair (x:y:xs) = ((x,y): pair xs)

main = do
    file <- readFile "challenge2.txt"
    let wires = read . head . words $ file
    let network = pair . map read . drop 2 . words $ file
    putStrLn $ if checkNetwork wires network then "Valid Network" else "Invalid Network"
