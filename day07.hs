import Common (processDay, splitBy)
import Data.List (sort)

type FuelCalculator = Int -> Int

-- For part 1, we can take the median of the list.
-- For part 2, I don't know if there is a smarter way of doing it,
-- so I just brute force the solution there.

result1 = processDay "07" implPart1

result2 :: IO Int
result2 = processDay "07" implPart2

implPart1 :: [String] -> Int
implPart1 lines = fuelTo calcPart1 midPoint crabs
  where
    crabs = readCrabs lines
    midPoint = notQuiteMedian crabs

notQuiteMedian :: [Int] -> Int
notQuiteMedian xs
  | odd n = sort xs !! mid
  | even n = sort xs !! mid - 1
  where
    n = length xs
    mid = n `div` 2

implPart2 :: [String] -> Int
implPart2 = findOptimum (-1) 0 . readCrabs

readCrabs :: [String] -> [Int]
readCrabs (c : _) = (fmap read . splitBy ',') c

findOptimum :: Int -> Int -> [Int] -> Int
findOptimum prevCost pos crabs
  | prevCost > currentCost || prevCost == -1 = findOptimum currentCost (pos + 1) crabs
  | otherwise = prevCost
  where
    currentCost = fuelTo calcPart2 pos crabs

fuelTo :: FuelCalculator -> Int -> [Int] -> Int
fuelTo calc dst = sum . fmap fuelCost
  where
    fuelCost x = calc . abs $ dst - x

calcPart1 :: FuelCalculator
calcPart1 = id

calcPart2 :: FuelCalculator
calcPart2 x = x * (x + 1) `div` 2
