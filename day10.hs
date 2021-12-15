import Common
import Data.Either (fromLeft, fromRight)
import Data.List (sort)

data Token a
  = Open a
  | Close a
  deriving (Show, Eq)

data Chunk
  = Parenthesis
  | Bracket
  | Brace
  | Comparison
  deriving (Show, Eq)

type ChunkToken = Token Chunk

type Stack = [Chunk]

result1 = processDay "10" (sum . fmap scoreOfLine)

result2 = processDay "10" (median . filter (/= 0) . sort . fmap autocompleteScore)
  where
    median x = x !! (length x `div` 2)

scoreOfLine :: String -> Int
scoreOfLine = fromLeft 0 . flip parseLine [] . fmap lexToken

autocompleteScore :: String -> Int
autocompleteScore = foldl f 0 . autocomplete . flip parseLine [] . fmap lexToken
  where
    f score chunk = score * 5 + autocompletionScoreOf chunk

autocomplete :: Either Int Stack -> Stack
autocomplete = fromRight []

parseLine :: [ChunkToken] -> Stack -> Either Int Stack
parseLine [] [] = Left 0 -- Line parses perfectly
parseLine [] stack = Right stack -- Line is incomplete
parseLine (t : ts) stack = parseToken stack t >>= parseLine ts
  where
    parseToken stack (Open x) = Right $ x : stack
    parseToken [] (Close x) = Left $ parsingScoreOf x
    parseToken (top : stack) (Close x)
      | top == x = Right stack
      | otherwise = Left $ parsingScoreOf x

lexToken :: Char -> ChunkToken
lexToken '(' = Open Parenthesis
lexToken '[' = Open Bracket
lexToken '{' = Open Brace
lexToken '<' = Open Comparison
lexToken ')' = Close Parenthesis
lexToken ']' = Close Bracket
lexToken '}' = Close Brace
lexToken '>' = Close Comparison

parsingScoreOf :: Chunk -> Int
parsingScoreOf Parenthesis = 3
parsingScoreOf Bracket = 57
parsingScoreOf Brace = 1197
parsingScoreOf Comparison = 25137

autocompletionScoreOf :: Chunk -> Int
autocompletionScoreOf Parenthesis = 1
autocompletionScoreOf Bracket = 2
autocompletionScoreOf Brace = 3
autocompletionScoreOf Comparison = 4