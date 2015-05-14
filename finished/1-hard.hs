-- http://www.reddit.com/r/dailyprogrammer/comments/pii6j/difficult_challenge_1/
-- A reverse guessing game


-- Changes range because guess number was too low
tooLowRange :: Int -> [Int] -> [Int]
tooLowRange guessNum range = dropWhile (<= guessNum) range

-- Returns new range from a guessed number that was too high
tooHighRange :: Int -> [Int] -> [Int]
tooHighRange guessNum range = takeWhile (< guessNum) range

newGuess :: [Int] -> Int
newGuess range = (head range) + (length range `div` 2)

getInput :: IO Char
getInput = getLine
    >>= \i -> if (head i) `elem` ['c','h','l']
        then return (head i)
        else getInput
    {-
    putStr ">>> "
    input <- getLine
    if (head (input ++ "as")) `elem` ['c','h','l']
        then return (head input)
        else getInput
    -}

handleInput :: Char -> (Int, [Int]) -> (Int, [Int])
handleInput 'c' (curNum, range) = (curNum, [])
handleInput 'h' (curNum, range) = (newNum, newRange)
    where
        newRange = tooHighRange curNum range
        newNum = newGuess newRange
handleInput 'l' (curNum, range) = (newNum, newRange)
    where
        newRange = tooLowRange curNum range
        newNum = newGuess newRange
--handleInput wrongInput numRange = numRange

printNumQuestion number = putStr "Is it " >> print number

guessLoop (curNum, range) = do
    printNumQuestion curNum
    input <- getInput
    let newNumRange = handleInput input (curNum, range)
    if null (snd newNumRange)
        then putStrLn "I knew it all along"
        else guessLoop newNumRange

main = do
    putStrLn "Hello. I'm here to guess your number [1..100]"
    putStrLn "too (h)igh, too (l)oo, (c)orrect"
    let curNum = 50
    let range = [1..100]
    guessLoop (curNum, range)
