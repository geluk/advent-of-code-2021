import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Grid

data BingoNumber
  = Unmarked Int
  | Marked Int
  deriving (Show, Eq)

type BingoBoard = Grid BingoNumber

-- left means a board won with the given score, right means the game continues
type RoundOutcome = Either Int [BingoBoard]

-- Utility function
readBoard :: [[Int]] -> [[BingoNumber]]
readBoard = fmap $ fmap Unmarked

playBingo :: [Int] -> [BingoBoard] -> RoundOutcome
playBingo (n : ns) = Right

playBingoRound :: Int -> [BingoBoard] -> RoundOutcome
playBingoRound number boards = sequence updatedBoards
  where
    updatedBoards = fmap updateBoard boards
    updateBoard board =
      let newBoard = tryMark number board
       in if wasWinningNumber number newBoard
            then (Left . calculateScore) newBoard
            else Right newBoard

calculateScore :: BingoBoard -> Int
calculateScore board = sum $ fmap sum scoreGrid
  where
    scoreGrid = gridMap scoreOfNumber board
    scoreOfNumber (Marked _) = 0
    scoreOfNumber (Unmarked x) = x

wasWinningNumber :: Int -> BingoBoard -> Bool
wasWinningNumber number board = maybe False (isWinningCoordinate board) maybeCoordinate
  where
    maybeCoordinate = findFirstInGrid (Marked number) board

isWinningCoordinate :: BingoBoard -> Coordinates -> Bool
isWinningCoordinate board (x, y) = allMarked colX && allMarked colY
  where
    colX = column x board
    colY = column y board
    allMarked = all isMarked
    isMarked (Unmarked _) = False
    isMarked (Marked _) = True

tryMark :: Int -> BingoBoard -> BingoBoard
tryMark num = fmap $ fmap markFn
  where
    markFn (Unmarked x) | x == num = Marked x
    markFn x = x
