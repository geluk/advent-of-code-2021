module Grid where

import Control.Monad (join)
import Data.List (elemIndices)
import Data.Map (Map, fromList, fromListWith)
import Data.Maybe (listToMaybe)

type Grid a = [[a]]

type Coordinates = (Int, Int)

gridMap :: (a -> b) -> Grid a -> Grid b
gridMap = fmap . fmap

column :: Int -> Grid a -> [a]
column x = fmap (!! x)

row :: Int -> Grid a -> [a]
row y grid = grid !! y

findFirstInGrid :: (Eq a) => a -> Grid a -> Maybe Coordinates
findFirstInGrid x = listToMaybe . findInGrid x

findInGrid :: (Eq a) => a -> Grid a -> [Coordinates]
findInGrid x grid = fp x grid 0

fp :: (Eq a) => a -> Grid a -> Int -> [Coordinates]
fp x [] _ = []
fp x (row : rows) i = findInRow row ++ fp x rows (i + 1)
  where
    findInRow row = zip (elemIndices x row) (repeat i)

toCoordinates :: Int -> Int -> Coordinates
toCoordinates x y = (x, y)
