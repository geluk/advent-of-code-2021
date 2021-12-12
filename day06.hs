import Common (processDay, splitBy)

type Fish = Int

result1 = processDay "06" $ implPart1 80

implPart1 :: Int -> [String] -> Int
implPart1 days (l : _) = length $ runSimulation days $ readFish l

readFish :: String -> [Fish]
readFish = fmap read . splitBy ','

-- This naïve implementation works fine for part 1,
-- but will need to be revised for part 2.
runSimulation :: Int -> [Fish] -> [Fish]
runSimulation 0 fish = fish
runSimulation x fish = runSimulation (x - 1) fish >>= simulateFish

simulateFish :: Fish -> [Fish]
simulateFish 0 = [6, 8]
simulateFish x = [x - 1]
