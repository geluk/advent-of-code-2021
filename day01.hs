import Common

-- Part 1
-- Should be 1564
result1 = processDay "01" implDay1part1

implDay1part1 :: [String] -> IO ()
implDay1part1 lines = do
    let numbers = read <$> lines
    print $ countIncreases numbers

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
-- Should be 1611
result2 = processDay "01" implDay1part2

implDay1part2 :: [String] -> IO ()
implDay1part2 lines = do
    let numbers = read <$> lines
    print $ countAverageIncreases numbers

countAverageIncreases :: [Int] -> Int
countAverageIncreases = countIncreases . makeAverages

makeAverages :: [Int] -> [Int]
makeAverages depths = zipWith3 sumThree depths tdepths ttdepths
    where
        tdepths = tail depths
        ttdepths = tail . tail $ depths

sumThree :: Int -> Int -> Int -> Int
sumThree x y z = x + y + z

-- Previous implementation
foldDepths :: [Int] -> Int
foldDepths x = fst $ foldl foldFunc (0, 9999999) x

foldFunc :: (Int, Int) -> Int -> (Int, Int)
foldFunc (acc, prev) current
    | current > prev = (acc + 1, current)
    | otherwise = (acc, current)

