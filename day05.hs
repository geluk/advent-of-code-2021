{-# LANGUAGE TupleSections #-}

import Common (processDay, replaceString, splitBy)
import Control.Monad (join, (>=>))
import Data.List (nub)
import Data.Maybe (mapMaybe)
import Data.Text (pack, replace, unpack)
import GHC.Types (Any)

type Coordinate = (Int, Int)

type Range = (Int, Int)

data Line
  = Horizontal {y :: Int, xRange :: Range}
  | Vertical {x :: Int, yRange :: Range}
  deriving (Show, Eq)

result1 = processDay "05" implPart1

implPart1 :: [String] -> Int
implPart1 = countIntersections . intersections . mapMaybe parseLine

parseLine :: String -> Maybe Line
parseLine = intoLine . fmap read . splitBy ',' . replaceString " -> " ","
  where
    intoLine [x1, y1, x2, y2] = createLine (x1, y1) (x2, y2)
    intoLine _ = error "malformed line coordinates"

createLine :: Coordinate -> Coordinate -> Maybe Line
createLine (x1, y1) (x2, y2)
  | x1 == x2 = Just Vertical {x = x1, yRange = (min y1 y2, max y1 y2)}
  | y1 == y2 = Just Horizontal {y = y1, xRange = (min x1 x2, max x1 x2)}
  | otherwise = Nothing

countIntersections :: [Coordinate] -> Int
countIntersections = length . nub

intersections :: [Line] -> [Coordinate]
intersections = combinations >=> uncurry intersects

combinations :: Eq a => [a] -> [(a, a)]
combinations [] = []
combinations (x : xs) = fmap (x,) xs ++ combinations xs

intersects :: Line -> Line -> [Coordinate]
intersects Horizontal {y = y1, xRange = r1} Horizontal {y = y2, xRange = r2}
  | y1 /= y2 = []
  | otherwise = (,y1) <$> matchParallel r1 r2
intersects Vertical {x = x1, yRange = r1} Vertical {x = x2, yRange = r2}
  | x1 /= x2 = []
  | otherwise = (x1,) <$> matchParallel r1 r2
intersects Vertical {x = x, yRange = yRange} Horizontal {y = y, xRange = xRange}
  | inRange yRange y && inRange xRange x = [(x, y)]
  | otherwise = []
intersects h v = intersects v h

inRange :: Range -> Int -> Bool
inRange (s, e) x = s <= x && x <= e

matchParallel :: Range -> Range -> [Int]
matchParallel (s1, e1) (s2, e2)
  | minE >= maxS = [maxS .. minE]
  | otherwise = []
  where
    minE = min e1 e2
    maxS = max s1 s2
