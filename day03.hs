import Common
import Data.List (foldl')

data Bit
  = Zero
  | One

type Report = [Bit]

type AccumulatedReport = [Int]

result1 = processDay "03" implPart1

implPart1 lines =
  let report = generateReport lines
   in gamma report * epsilon report

generateReport :: [String] -> AccumulatedReport
generateReport = foldReports . parseReports

toDecimal :: [Int] -> Int
toDecimal = foldl' (\acc x -> acc * 2 + x) 0

gamma :: AccumulatedReport -> Int
gamma = toDecimal . fmap gammaBit

epsilon :: AccumulatedReport -> Int
epsilon = toDecimal . fmap (gammaBit . negate)

gammaBit :: Int -> Int
gammaBit x
  | x < 0 = 0
  | x > 0 = 1
  | otherwise = error "1 and 0 equally common"

parseReports :: [String] -> [Report]
parseReports = fmap parseReport

parseReport :: String -> Report
parseReport = fmap parseBit

parseBit :: Char -> Bit
parseBit '0' = Zero
parseBit '1' = One

foldReports :: [Report] -> AccumulatedReport
foldReports = foldl zipReports $ repeat 0

zipReports :: AccumulatedReport -> Report -> AccumulatedReport
zipReports = zipWith applyBit

applyBit :: Int -> Bit -> Int
applyBit acc One = acc + 1
applyBit acc Zero = acc - 1