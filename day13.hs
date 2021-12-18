{-# LANGUAGE ViewPatterns #-}

import Common
import Control.Monad (join)
import Data.Foldable (traverse_)
import Data.List (foldl', stripPrefix)
import Data.Set as Set (Set, filter, findMax, fromList, map, null, size)
import Grid (Coordinates)

data FoldInstruction
  = AlongX Int
  | AlongY Int
  deriving (Show)

type Paper = Set Coordinates

result1 = processDay "13" $ Set.size . implPart1

result2 = join $ processDay "13" implPart2

implPart1 lines = applyFolds paper [i]
  where
    (paper, i : is) = parseInput lines

-- traverse_ is like a regular traverse,
-- but discards the results.
implPart2 = traverse_ print . plotCoordinates 0 . uncurry applyFolds . parseInput

plotCoordinates :: Int -> Paper -> [String]
plotCoordinates pos coords
  | pos > maxY = []
  | Set.null xs = [] : plotCoordinates (pos + 1) coords
  | otherwise = plotLine 0 xs : plotCoordinates (pos + 1) coords
  where
    xs = Set.map fst . Set.filter ((== pos) . snd) $ coords
    maxY = findMax $ Set.map snd coords

plotLine :: Int -> Set Int -> String
plotLine pos coords
  | pos > maxX = []
  | pos `elem` coords = '#' : plotLine (pos + 1) coords
  | otherwise = ' ' : plotLine (pos + 1) coords
  where
    maxX = findMax coords

applyFolds :: Paper -> [FoldInstruction] -> Paper
applyFolds = foldl' $ flip applyInstruction

parseInput :: [String] -> (Paper, [FoldInstruction])
parseInput lines = (Set.fromList $ parseCoordinate <$> coordinates, parseInstruction <$> instructions)
  where
    parseCoordinate = intoCoordinate . splitBy ','
    intoCoordinate [x, y] = (read x, read y)
    [coordinates, instructions] = splitBy [] lines

parseInstruction :: String -> FoldInstruction
parseInstruction (stripPrefix "fold along x=" -> Just x) = AlongX $ read x
parseInstruction (stripPrefix "fold along y=" -> Just y) = AlongY $ read y

applyInstruction :: FoldInstruction -> Paper -> Paper
applyInstruction (AlongX x) = Set.map $ foldAlongX x
applyInstruction (AlongY y) = Set.map $ foldAlongY y

foldAlong :: Int -> Int -> Int
foldAlong l x
  | x > l = x + (dX * 2)
  -- We should only reflect points from the outer field
  -- to the inner field, not the other way.
  | otherwise = x
  where
    dX = l - x

foldAlongY :: Int -> Coordinates -> Coordinates
foldAlongY l (x, y) = (x, foldAlong l y)

foldAlongX :: Int -> Coordinates -> Coordinates
foldAlongX l (x, y) = (foldAlong l x, y)
