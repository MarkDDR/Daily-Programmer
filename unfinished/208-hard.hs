-- http://www.reddit.com/r/dailyprogrammer/comments/31aja8/20150403_challenge_208_hard_the_universal_machine/
-- Simulates a Turing Machine
import Data.Maybe


type State = String
type Tape = String
type Head = Int
type TapeMiddle = Int
type GoRight = Boolean -- True == Right, False == Left
type Input = Char
type Output = (State, Char, GoRight)
type SysState = (Tape, Head, State)
-- Defines every input and output for a single state
type StateDef = (State, [Input], [Output])


updateSysState :: [StateDef] -> SysState -> SysState
updateSysState stateDefs curState = applyStateDef (getStateDef (thr3 curState) stateDefs) curState

fst3 (a,_,_) = a
snd3 (_,b,_) = b
thr3 (_,_,c) = c

-- If there is an error, there is a general problem with the program
getStateDef :: State -> [StateDef] -> StateDef
getStateDef state (x:xs)
    | fst3 x == state   = x
    | otherwise         = getStateDef state xs

-- Input index corresponds to Output index
-- elemIndex shoudl never return "Nothing", otherwise there is a problem
getInputIndex :: Input -> [Input] -> Int
getInputIndex = fromJust . elemIndex

applyStateDef :: StateDef -> SysState -> SysState
applyStateDef (_, inputs, outputs) (tape, headNum, _) =
    let tHead = getHead tape headNum
        inputIndex = getInputIndex tHead inputs
        output = outputs !! inputIndex
        nTape = newTape tape headNum (snd3 output)
        newState = fst3 output
        newHead = if thr3 output then headNum + 1 else headNum - 1
    in (nTape, newState, newHead)

-- Head can't be negative
newTape :: Tape -> Head -> Char -> Tape
newTape [] _ = []
newTape (x:xs) 0 newChar = (newChar:xs)
newTape curTape head newChar = newTape curTape (head - 1) newChar

-- Doesn't account for out-of-bounds
-- Proper Tape management should make OoB impossible though
getHead :: Tape -> Head -> Char
getHead tape headNum = tape !! headNum

---------------

-- Make sure we don't fall off the edge of the tape
-- Int keeps track of how much middle has moved
fixTapeAndHead :: SysState -> (SysState, Int)
fixTapeAndHead (curTape, curHead, state)
    | curHead == 0                  = (("_" ++ curTape, curHead + 1, state), 1)
    | curHead - 1 == length curTape = ((curTape ++ "_", curHead, state), 0)
    | otherwise                     = ((curTape, curHead, state), 0)


-- Assume Tape has _ on either end
runMachine :: State -> [StateDefs] -> SysState -> TapeMiddle -> (Tape, Head, TapeMiddle)
runMachine endState stateDefs curSysState tapeMiddle
    | thr3 newState == endState     = (fst3 newState, snd3 newState, newTapeMiddle)
    | otherwise                     = runMachine endState stateDefs newState newTapeMiddle
    where
        newStateAndTape = fixTapeAndHead (updateSysState stateDefs curSysState)
        newState = fst newStateAndTape
        newTapeMiddle = snd newStateAndTape

-- TODO: Implement parse

parseStateDefs :: String -> 

main = do
    let initTape = "_01100100_"
    let initState = "Init"
    let endState = "OK"
    let stateDefs =
