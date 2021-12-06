import Common
import Data.Functor

-- Part 1
result1 :: IO Int
result1 = processDay "01" implPart1

implPart1 :: [String] -> Int
implPart1 = countIncreases . readLines

readLines :: [String] -> [Int]
readLines lines = read <$> lines

countIncreases :: [Int] -> Int
countIncreases = sum . identifyIncreases

identifyIncreases :: [Int] -> [Int]
identifyIncreases depths = zipWith isIncrease depths next
    where _:next = depths

isIncrease :: Int -> Int -> Int
isIncrease l r
    | l < r = 1
    | otherwise = 0

-- Part 2
result2 = processDay "01" implPart2

implPart2 :: [String] -> Int
implPart2 = countAverageIncreases . readLines

countAverageIncreases :: [Int] -> Int
countAverageIncreases = countIncreases . makeAverages

makeAverages :: [Int] -> [Int]
makeAverages depths = zipWith3 sumThree depths tdepths ttdepths
    where
        tdepths = tail depths
        ttdepths = tail . tail $ depths

sumThree :: Int -> Int -> Int -> Int
sumThree x y z = x + y + z