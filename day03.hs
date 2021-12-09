import Common
import Data.List (foldl')
import Data.Maybe (mapMaybe)

data Bit
  = Zero
  | One
  deriving (Eq, Show)

type Report = [Bit]

type AccumulatedReport = [Int]

-- Part 1
result1 = processDay "03" implPart1

implPart1 lines =
  let report = generateReport lines
   in gamma report * epsilon report

gamma :: AccumulatedReport -> Int
gamma = toDecimal . fmap findMostCommonBit

epsilon :: AccumulatedReport -> Int
epsilon = toDecimal . fmap (findMostCommonBit . negate)

toDecimal :: [Bit] -> Int
toDecimal = foldl' (\acc x -> acc * 2 + toInt x) 0
  where
    toInt Zero = 0
    toInt One = 1

findMostCommonBit :: Int -> Bit
findMostCommonBit x
  | x < 0 = Zero
  | x > 0 = One
  | otherwise = error "1 and 0 equally common"

generateReport :: [String] -> AccumulatedReport
generateReport = foldReports . parseReports

parseReports :: [String] -> [Report]
parseReports = fmap $ fmap parseBit
  where
    parseBit '0' = Zero
    parseBit '1' = One

foldReports :: [Report] -> AccumulatedReport
foldReports = foldl accumulateReportBits $ repeat 0
  where
    accumulateReportBits = zipWith $ flip accumulateBit
    accumulateBit One = (+ 1)
    accumulateBit Zero = subtract 1

-- Part 2
result2 = processDay "03" implPart2

implPart2 lines =
  oxygen * co2
  where
    reports = parseReports lines
    oxygen = toDecimal $ lifeSupport oxygenCriteria reports
    co2 = toDecimal $ lifeSupport co2Criteria reports

lifeSupport :: (Int -> Bit) -> [Report] -> [Bit]
lifeSupport _ [x] = x
lifeSupport _ ([] : _) = []
lifeSupport criteriaFn reports = requiredBit : lifeSupport criteriaFn matchedSubReports
  where
    requiredBit = criteriaFn . head $ foldReports reports
    matchedSubReports = mapMaybe predicate reports
    predicate (bit : bits)
      | bit == requiredBit = Just bits
      | otherwise = Nothing

oxygenCriteria :: Int -> Bit
oxygenCriteria x
  | x < 0 = Zero
  | x >= 0 = One

co2Criteria :: Int -> Bit
co2Criteria x
  | x < 0 = One
  | x >= 0 = Zero
