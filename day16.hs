import Common (processDay)
import Control.Monad (ap, liftM)
import Data.List (foldl')
import Data.Map ((!))
import qualified Data.Map as Map
import Text.Show.Functions ()

-- Packet type definitions --
-- ======================= --
data Packet = Packet {version :: Int, ptype :: PacketType}
  deriving (Show)

data PacketType
  = Operator Op [Packet]
  | Literal Int
  deriving (Show)

data Op
  = Comparison (Int -> Int -> Int)
  | Semigroup (Int -> Int -> Int)
  deriving (Show)

data Bit
  = O
  | I
  deriving (Show, Eq, Ord)

-- Parser type definitions --
-- ======================= --
newtype Parser a = Parser {parse :: ParserState -> ParseResults a}

type ParserState = (Position, BitString)

type Position = Int

type BitString = [Bit]

type ParseResults a = [(a, ParserState)]

instance Monad Parser where
  return x = Parser $ \s -> [(x, s)]
  (Parser p) >>= f = Parser $ \s ->
    let pResults = p s
     in concatMap (\(x, s') -> parse (f x) s') pResults

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Functor Parser where
  fmap = liftM

-- Implementation --
-- ============== --
result1 = processDay "16" $ versionSum . parseInput

result2 = processDay "16" $ eval . parseInput

parseInput :: [String] -> Packet
parseInput line = fst . head $ parse readPacket (0, head line >>= (lookup !))
  where
    lookup = Map.fromList $ zip chars bits
    chars = ['0' .. '9'] ++ ['A' .. 'F']
    bits = [[a, b, c, d] | a <- [O, I], b <- [O, I], c <- [O, I], d <- [O, I]]

eval :: Packet -> Int
eval Packet {ptype = pt} =
  case pt of
    (Operator (Comparison op) [l, r]) -> eval l `op` eval r
    (Operator (Semigroup op) pkts) -> foldr1 op $ eval <$> pkts
    (Literal x) -> x

versionSum :: Packet -> Int
versionSum Packet {version = v, ptype = pt} =
  case pt of
    (Operator _ subpkts) -> sum $ v : (versionSum <$> subpkts)
    (Literal _) -> v

-- Packet parsers --
-- ============== --

-- Read a single packet with its subpackets
readPacket :: Parser Packet
readPacket = do
  (version, ptypeId) <- readHeader
  case ptypeId of
    4 -> constructLiteralPacket version <$> readLiteral
    _ -> readOperatorPacket version $ getOp ptypeId
  where
    getOp 0 = Semigroup (+)
    getOp 1 = Semigroup (*)
    getOp 2 = Semigroup min
    getOp 3 = Semigroup max
    getOp 5 = Comparison $ cond (>)
    getOp 6 = Comparison $ cond (<)
    getOp 7 = Comparison $ cond (==)
    cond p l r = if p l r then 1 else 0

readHeader :: Parser (Int, Int)
readHeader = (,) <$> readInt 3 <*> readInt 3

-- Construct a literal packet using the provided version and literal value
constructLiteralPacket :: Int -> Int -> Packet
constructLiteralPacket version value = Packet {version = version, ptype = Literal value}

-- Read an operator packet using the provided version and operator
readOperatorPacket :: Int -> Op -> Parser Packet
readOperatorPacket version op = do
  lType <- readBit
  subPackets <- case lType of
    O -> readLtypeSubpackets
    I -> readNtypeSubpackets
  return Packet {version = version, ptype = Operator op subPackets}

-- Read the N-type header, then read N subpackets
readNtypeSubpackets :: Parser [Packet]
readNtypeSubpackets = do
  n <- readInt 11
  manyN n readPacket

-- Read the L-type header, then read subpackets until we pass pos + L
readLtypeSubpackets :: Parser [Packet]
readLtypeSubpackets = do
  endPos <- (+) <$> readInt 15 <*> getPosition
  let readIfNotExceeded = failWhen (>= endPos) getPosition *> readPacket
  many0 readIfNotExceeded

-- Read a literal value
readLiteral :: Parser Int
readLiteral = toDecimal <$> readLiteralBits

-- Keep reading literal bits until we encounter the final chunk
readLiteralBits :: Parser [Bit]
readLiteralBits = do
  chunk <- readBits 5
  case chunk of
    (I : quad) -> (quad ++) <$> readLiteralBits
    (O : quad) -> return quad

-- Read an N-bit integer
readInt :: Int -> Parser Int
readInt n = toDecimal <$> readBits n

-- Read a single bit
readBit :: Parser Bit
readBit = Parser $ \s@(pos, bit : rest) -> [(bit, (pos + 1, rest))]

-- Read N bits
readBits :: Int -> Parser [Bit]
readBits n = manyN n readBit

-- Returns the current position without advancing the parser.
getPosition :: Parser Position
getPosition = Parser $ \s@(pos, _) -> [(pos, s)]

-- Generic parser combinator functions --
-- =================================== --

-- Construct a parser that fails if a predicate fails on the result of a subparser
failWhen :: (a -> Bool) -> Parser a -> Parser a
failWhen p x = do
  rx <- x
  if p rx then failed else return rx

-- Repeat a parser until it fails
many0 :: Parser a -> Parser [a]
many0 x = do
  willFail <- fails x
  if willFail
    then return []
    else (:) <$> x <*> many0 x

-- Repeat a parser N times
manyN :: Int -> Parser a -> Parser [a]
manyN 1 p = fmap (: []) p
manyN n p = (:) <$> p <*> manyN (n - 1) p

-- Use a combination function to combine the outcome of two parsers
combineWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
combineWith f p1 p2 = f <$> p1 <*> p2

-- Returns true if a parser will fail
fails :: Parser a -> Parser Bool
fails p = Parser $ \s -> [(null $ parse p s, s)]

-- Constructs a parser that
failed :: Parser a
failed = Parser $ const []

-- Utility functions --
-- ================= --

toDecimal :: [Bit] -> Int
toDecimal = foldl' (\acc x -> acc * 2 + toInt x) 0
  where
    toInt O = 0
    toInt I = 1
