-- http://www.reddit.com/r/dailyprogrammer/comments/32vlg8/20150417_challenge_210_hard_loopy_robots/
--

import Data.Either

-- 0 is North
-- 1 is West
-- 2 is South
-- 3 is East
type Direction = Int
-- DeltaDirection goes counter-clockwise, although shouldn't matter as long as implementation is consistant
-- DeltaDirection:
-- 0 = No change
-- 1 = add 90
-- 2 = add 180
-- 3 = add 270
-- use mod 4
type DeltaDirection = Int
type DeltaCoord = (Int, Int) -- (Forward, Rightward)
type Coord = (Int, Int) -- (x,y)
type State = (Coord, Direction)
type CmdChange = (DeltaCoord, DeltaDirection)


(t+) :: (a,b) -> (a,b) -> (a,b)
(a,b) t+ (c,d) = (a+c, b+d)

simplifyCmdEZ :: String -> CmdChange
simplifyCmdEZ cmdStr = simplifyCmd cmdStr ((0,0),0)

simplifyCmd :: String -> CmdChange -> CmdChange
simplifyCmd [] cmd = cmd
simplifyCmd (x:xs) cmdChg = simplifyCmd xs cmdChg
simplifyCmd ('S':xs) (dCoord, dDir) =
    let stepDir = stepDCoord dDir
    let newCmd = (dCoord t+ stepDir, dDir)
    in simplifyCmd xs newCmd
simplifyCmd ('R':xs) (dCoord, dDir) = simplifyCmd xs (dCoord, (dDir - 1) `mod` 4)
simplifyCmd ('L':xs) (dCoord, dDir) = simplifyCmd xs (dCoord, (dDir + 1) `mod` 4)

stepDCoord :: DeltaDirection -> DeltaCoord
stepDCoord 0 = (1,0)
stepDCoord 1 = (0,-1)
stepDCoord 2 = (-1,0)
stepDCoord 3 = (0,1)
stepDCoord x = stepDCoord (x `mod` 4)

------------------------------

updateState :: CmdChange -> State -> State
updateState (dCoord, dDir) (location, dir) = (newLocation, newDirection)
    where
        newLocation = location t+ (dCoordtoCoord dCoord)
        newDirection = (dDir + dir) `mod` 4

dCoordToCoord :: Direction -> DeltaCoord -> Coord
dCoordToCoord 0 (f,r) = (f,r)
dCoordToCoord 3 (f,r)  = (-f,r)
dCoordToCoord 2 (f,r) = (-f,-r)
dCoordToCoord 1 (f,r)  = (f,-r)

-----------------------------
--
-- Loop Checks

-- If the direction doesn't change, then it will not loop
noLoopCheckA :: CmdChange -> Bool
noLoopCheckA cmd = if snd cmd == 0 then True else False

-- 
noLoopCheckB
