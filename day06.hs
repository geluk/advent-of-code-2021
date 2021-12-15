{-# LANGUAGE TupleSections #-}

import Common (processDay, splitBy)
import Data.List (sort)
import Data.Map (Map, delete, elems, fromList, fromListWith, mapKeys, toList, union, (!?))
import Data.Maybe (maybeToList)

type Count = Int

type Generation = Int

type School = Map Generation Count

result1 = processDay "06" $ impl 80

result2 = processDay "06" $ impl 256

impl :: Int -> [String] -> Int
impl days = sum . elems . runSimulation days . readFish . head

readFish :: String -> School
readFish line = fromListWith (+) $ (,1) <$> ages line
  where
    ages = fmap read . splitBy ','

runSimulation :: Int -> School -> School
runSimulation 0 school = school
runSimulation x school = runSimulation (x - 1) nextGeneration
  where
    shifted = mapKeys (subtract 1) school
    parentCount = maybeToList $ shifted !? (-1)
    parents = (6,) <$> parentCount
    children = (8,) <$> parentCount
    cleaned = toList . delete (-1) $ shifted
    nextGeneration = fromListWith (+) (cleaned ++ parents ++ children)