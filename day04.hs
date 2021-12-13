import Common
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Grid

data BingoNumber
  = Unmarked Int
  | Marked Int
  deriving (Show, Eq)

type BingoBoard = Grid BingoNumber

-- Left means a board won with the given score, right means the game continues.
-- Using the Either monad like this means we can bind the 'play round' function
-- onto the game state repeatedly until it collapses into the left state,
-- producing a score.
type RoundOutcome = Either Int [BingoBoard]

-- A strategy determines the outcome of a round, by choosing whether the game
-- should continue. This will be relevant for part 2.
type Strategy = [Either Int BingoBoard] -> RoundOutcome

result1 = processDay "04" $ impl tryToWin

result2 = processDay "04" $ impl tryToLose

impl :: Strategy -> [String] -> RoundOutcome
impl strategy (line : lines) = playBingo strategy numbers boards
  where
    numbers = readNumbers line
    boards = readBoards lines

readBoards :: [String] -> [BingoBoard]
readBoards = fmap (readBoard . parseBoard) . filter (not . null) . splitBy []
  where
    parseBoard = fmap parseLine
    parseLine = fmap read . filter (not . null) . splitBy ' '

readNumbers :: String -> [Int]
readNumbers line = read <$> splitBy ',' line

readBoard :: [[Int]] -> [[BingoNumber]]
readBoard = gridMap Unmarked

playBingo :: Strategy -> [Int] -> [BingoBoard] -> RoundOutcome
playBingo _ [] boards = Right boards
playBingo strat (n : ns) boards = playBingoRound strat n boards >>= playBingo strat ns

playBingoRound :: Strategy -> Int -> [BingoBoard] -> RoundOutcome
playBingoRound strat number boards = strat updatedBoards
  where
    updatedBoards = updateBoard number <$> boards

tryToWin :: Strategy
tryToWin = sequence

tryToLose :: Strategy
tryToLose [Left score] = Left score
tryToLose boards = sequence $ filter didNotWin boards
  where
    didNotWin (Right _) = True
    didNotWin (Left _) = False

updateBoard :: Int -> BingoBoard -> Either Int BingoBoard
updateBoard number board =
  let newBoard = tryMark number board
   in if wasWinningNumber number newBoard
        then (Left . (* number) . calculateScore) newBoard
        else Right newBoard

calculateScore :: BingoBoard -> Int
calculateScore board = sum $ sum <$> scoreGrid
  where
    scoreGrid = gridMap scoreOfNumber board
    scoreOfNumber (Marked _) = 0
    scoreOfNumber (Unmarked x) = x

wasWinningNumber :: Int -> BingoBoard -> Bool
wasWinningNumber number board = maybe False (isWinningCoordinate board) maybeCoordinate
  where
    maybeCoordinate = findFirstInGrid (Marked number) board

isWinningCoordinate :: BingoBoard -> Coordinates -> Bool
isWinningCoordinate board (x, y) = allMarked colX || allMarked rowY
  where
    colX = column x board
    rowY = row y board
    allMarked = all isMarked
    isMarked (Unmarked _) = False
    isMarked (Marked _) = True

tryMark :: Int -> BingoBoard -> BingoBoard
tryMark num = gridMap markFn
  where
    markFn (Unmarked x) | x == num = Marked x
    markFn x = x
