import Common

-- Part 1
result1 = processDay "01" implPart1

implPart1 :: [String] -> IO ()
implPart1 lines = do
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
result2 = processDay "01" implPart2

implPart2 :: [String] -> IO ()
implPart2 lines = do
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