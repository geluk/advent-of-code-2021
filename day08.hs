import Common (processDay, splitOn)
import Control.Monad (join)
import Data.Char (toUpper)
import Data.List (find, sort)
import Data.Map as Map

data Segment
  = A
  | B
  | C
  | D
  | E
  | F
  | G
  deriving (Eq, Ord, Show, Read)

type Pattern = [Segment]

type Decoder = Map Segment Segment

result1 = processDay "08" implPart1

implPart1 :: [String] -> Int
implPart1 = length . (>>= singleLine)
  where
    singleLine = Prelude.filter (`elem` [1, 4, 7, 8]) . resolveDisplayedDigits

result2 = processDay "08" implPart2

implPart2 :: [String] -> Int
implPart2 = sum . fmap singleLine
  where
    singleLine = read . concatMap show . resolveDisplayedDigits

parseLine :: String -> ([Pattern], [Pattern])
parseLine line = (readPattern <$> words left, readPattern <$> words right)
  where
    [left, right] = splitOn " | " line
    readPattern = fmap readSegment

resolveDisplayedDigits :: String -> [Int]
resolveDisplayedDigits line = fmap (decodeDigit decoder) digits
  where
    (signals, digits) = parseLine line
    decoder = solveLine signals

readSegment :: Char -> Segment
readSegment x = read [toUpper x]

decodeDigit :: Decoder -> Pattern -> Int
decodeDigit decoder = render . sort . fmap decodeSegment
  where
    decodeSegment x = decoder ! x
    render [A, B, C, E, F, G] = 0
    render [C, F] = 1
    render [A, C, D, E, G] = 2
    render [A, C, D, F, G] = 3
    render [B, C, D, F] = 4
    render [A, B, D, F, G] = 5
    render [A, B, D, E, F, G] = 6
    render [A, C, F] = 7
    render [A, B, C, D, E, F, G] = 8
    render [A, B, C, D, F, G] = 9
    render _ = error "Malformed digit"

solveLine :: [Pattern] -> Decoder
solveLine lines = createDecoder four counts
  where
    counts = fromListWith (+) [(x, 1) | x <- join lines]
    Just four = find ((== 4) . length) lines

-- The number four is the key to resolving ambiguous segment patterns.
-- Given the segment pattern of the number four, and a map indicating
-- how often we've seen each segment, we can construct a decoder.
createDecoder :: Pattern -> Map Segment Int -> Decoder
createDecoder fourPattern segmentCounts = fromList [(a, A), (b, B), (c, C), (d, D), (e, E), (f, F), (g, G)]
  where
    [a, c] = disambiguateAC $ hasCount 8
    [b] = hasCount 6
    [d, g] = disambiguateDG $ hasCount 7
    [e] = hasCount 4
    [f] = hasCount 9
    hasCount x = keys $ Map.filter (== x) segmentCounts
    disambiguateAC [x, y] = if x `elem` fourPattern then [y, x] else [x, y]
    disambiguateDG [x, y] = if x `elem` fourPattern then [x, y] else [y, x]
