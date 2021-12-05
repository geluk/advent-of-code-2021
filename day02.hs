{-# LANGUAGE ViewPatterns #-}
import Data.List (stripPrefix)
import Common

-- Part 1
data Command
    = Forward Int
    | Down Int
    deriving (Show)

type Position = (Int, Int)

result1 = processDay "02" implDay2part1

implDay2part1 :: [String] -> IO ()
implDay2part1 = print . multTuple . foldCommands . parseCommands

multTuple :: (Int, Int) -> Int
multTuple (x, y) = x * y

foldCommands :: [Command] -> Position
foldCommands = foldl foldFunc (0, 0)

foldFunc :: Position -> Command -> Position
foldFunc (x, y) (Forward dx) = (x + dx, y)
foldFunc (x, y) (Down dy) = (x, y + dy)

parseCommands :: [String] -> [Command]
parseCommands commands = parseCommand <$> commands

parseCommand :: String -> Command
parseCommand (stripPrefix "forward" -> Just x) = Forward $ read x
parseCommand (stripPrefix "down" -> Just x) = Down $ read x
parseCommand (stripPrefix "up" -> Just x) = up $ read x

up :: Int -> Command
up = Down . negate

-- Part 2
-- (x, y, aim)
type AimPosition = (Int, Int, Int)

result2 = processDay "02" implDay2part2

implDay2part2 = print . multAimPos . foldWithAim . parseCommands

multAimPos :: (Int, Int, Int) -> Int
multAimPos (x, y, _) = x * y

foldWithAim :: [Command] -> AimPosition
foldWithAim = foldl foldWithAimFunc (0, 0, 0)

foldWithAimFunc ::  AimPosition -> Command -> AimPosition
foldWithAimFunc (x, y, a) (Forward dx) = (x+dx, y+a*dx, a)
foldWithAimFunc (x, y, a) (Down da) = (x, y, a+da)