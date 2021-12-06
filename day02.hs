{-# LANGUAGE ViewPatterns #-}
import Data.List (stripPrefix)
import Common

-- Part 1
data Command
    = Forward Int
    | Down Int
    deriving (Show)

type Position = (Int, Int) -- (x, y)

result1 = processDay "02" part1Evaluator

part1Evaluator :: [String] -> Int
part1Evaluator = multTuple . foldCommands . parseCommands

multTuple :: (Int, Int) -> Int
multTuple (x, y) = x * y

foldCommands :: [Command] -> Position
foldCommands = foldl evaluateCommand (0, 0)

evaluateCommand :: Position -> Command -> Position
evaluateCommand (x, y) (Forward dx) = (x + dx, y)
evaluateCommand (x, y) (Down dy) = (x, y + dy)

parseCommands :: [String] -> [Command]
parseCommands commands = parseCommand <$> commands

parseCommand :: String -> Command
parseCommand (stripPrefix "forward" -> Just x) = Forward $ read x
parseCommand (stripPrefix "down" -> Just x) = Down $ read x
parseCommand (stripPrefix "up" -> Just x) = up $ read x

up :: Int -> Command
up = Down . negate

-- Part 2
type AimPosition = (Int, Int, Int) -- (x, y, aim)

result2 = processDay "02" part2Evaluator

part2Evaluator :: [String] -> Int
part2Evaluator = multAimPos . foldWithAim . parseCommands

multAimPos :: (Int, Int, Int) -> Int
multAimPos (x, y, _) = x * y

foldWithAim :: [Command] -> AimPosition
foldWithAim = foldl foldWithAimFunc (0, 0, 0)

foldWithAimFunc ::  AimPosition -> Command -> AimPosition
foldWithAimFunc (x, y, a) (Forward dx) = (x+dx, y+a*dx, a)
foldWithAimFunc (x, y, a) (Down da) = (x, y, a+da)