{-# LANGUAGE TupleSections #-}

import Common (processDay, replaceString, splitBy)
import Control.Monad (join, (>=>))
import Data.List (intersect, nub)
import Data.Maybe (mapMaybe)
import Data.Text (pack, replace, unpack)
import GHC.Types (Any)

type Coordinate = (Int, Int)

type Range = (Int, Int)

data Line
  = Horizontal {y :: Int, xRange :: Range}
  | Vertical {x :: Int, yRange :: Range}
  | DiagonalUp {start :: Coordinate, up :: Int}
  | DiagonalDown {start :: Coordinate, down :: Int}
  deriving (Show, Eq)

-- This solution isn't ideal, because you either need to pattern match
-- against every combination of lines, or you need to plot both lines and
-- intersect their coordinates. The former is unwieldy but fast, the latter
-- is simpler to implement, but slow. In this solution, I use a combination
-- of both approaches, but neither is really ideal.
-- In hindsight, it might have been better to plot each line
-- to a `Map Coordinate Int`, and then counting all entries with a value
-- greather than 1, but that seemed less interesting.
result1 = processDay "05" implPart1

implPart1 :: [String] -> Int
implPart1 = countIntersections . intersections . mapMaybe (dropDiagonal . parseLine)

implPart2 :: [String] -> Int
implPart2 = countIntersections . intersections . fmap parseLine

dropDiagonal :: Line -> Maybe Line
dropDiagonal DiagonalUp {} = Nothing
dropDiagonal DiagonalDown {} = Nothing
dropDiagonal x = Just x

parseLine :: String -> Line
parseLine = intoLine . fmap read . splitBy ',' . replaceString " -> " ","
  where
    intoLine [x1, y1, x2, y2] = createLine (x1, y1) (x2, y2)
    intoLine _ = error "malformed line coordinates"

createLine :: Coordinate -> Coordinate -> Line
createLine (x1, y1) (x2, y2)
  | x1 == x2 = Vertical {x = x1, yRange = (min y1 y2, max y1 y2)}
  | y1 == y2 = Horizontal {y = y1, xRange = (min x1 x2, max x1 x2)}
  | y1 < y2 && x1 < x2 = DiagonalUp {start = (x1, y1), up = y2 - y1}
  | y1 > y2 && x1 < x2 = DiagonalDown {start = (x1, y1), down = y1 - y2}
  | y1 < y2 && x1 > x2 = DiagonalDown {start = (x2, y2), down = y2 - y1}
  | y1 > y2 && x1 > x2 = DiagonalUp {start = (x2, y2), up = y1 - y2}

countIntersections :: [Coordinate] -> Int
countIntersections = length . nub

intersections :: [Line] -> [Coordinate]
intersections = combinations >=> uncurry intersects

combinations :: Eq a => [a] -> [(a, a)]
combinations [] = []
combinations (x : xs) = fmap (x,) xs ++ combinations xs

-- As explained above, this function turned out to be a bit troublesome.
-- Ideally it would just plot both lines and intersect them (as you can see
-- in final pattern match), but that doesn't turn out to be fast enough,
-- so we also perform some pattern matching against common cases.
-- If we pattern matched them all though, this function would need 16 pattern
-- match branches. We could make a few short cuts (as we do here by dispatching
-- Horizontal + Vertical to Vertical + Horizontal), but at the very least you'd
-- still need to write out those branches.
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
intersects h@Horizontal {} v@Vertical {} = intersects v h
intersects l1 l2 = plot l1 `intersect` plot l2

inRange :: Range -> Int -> Bool
inRange (s, e) x = s <= x && x <= e

plot :: Line -> [Coordinate]
plot Horizontal {y = y, xRange = (start, end)} = (,y) <$> [start .. end]
plot Vertical {x = x, yRange = (start, end)} = (x,) <$> [start .. end]
plot DiagonalUp {start = (x, y), up = u} = [(x + du, y + du) | du <- [0 .. u]]
plot DiagonalDown {start = (x, y), down = d} = [(x + dd, y - dd) | dd <- [0 .. d]]

matchParallel :: Range -> Range -> [Int]
matchParallel (s1, e1) (s2, e2)
  | minE >= maxS = [maxS .. minE]
  | otherwise = []
  where
    minE = min e1 e2
    maxS = max s1 s2
