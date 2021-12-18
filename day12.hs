import Common
import Control.Monad (msum)
import Data.Char (isUpper)
import Data.Map as Map (Map, empty, insertWith, member, (!), (!?))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set as Set (Set, empty, insert, member, singleton, toList, union)

data Cave
  = Start
  | End
  | Small String
  | Big String
  deriving (Eq, Ord, Show)

type CaveGraph = Map Cave Neighbours

type Neighbours = Set Cave

type Seen = Map Cave Int

type TraversalCriteria = Seen -> Cave -> Bool

result1 = processDay "12" $ countConnections strategy1 . buildGraph

-- TODO: this works, but it's way too slow.
result2 = processDay "12" $ countConnections strategy2 . buildGraph

countConnections :: TraversalCriteria -> CaveGraph -> Int
countConnections criteria graph = paths criteria graph Map.empty Start

buildGraph :: [String] -> CaveGraph
buildGraph xs = foldr1 (.) (insertConnection . parseConnection <$> xs) Map.empty

parseConnection :: String -> (Cave, Cave)
parseConnection = toTuple . fmap parseCave . splitBy '-'
  where
    toTuple [a, b] = (a, b)

parseCave :: String -> Cave
parseCave "start" = Start
parseCave "end" = End
parseCave x
  | all isUpper x = Big x
  | otherwise = Small x

insertConnection :: (Cave, Cave) -> CaveGraph -> CaveGraph
insertConnection (x, y) = insertOneWay x y . insertOneWay y x
  where
    insertOneWay k v = Map.insertWith Set.union k (Set.singleton v)

paths :: TraversalCriteria -> CaveGraph -> Seen -> Cave -> Int
paths _ _ _ End = 1
paths mayTraverse conns seen cave
  | mayTraverse seen cave = sum $ nextPaths <$> Set.toList edges
  | otherwise = 0
  where
    nextPaths = paths mayTraverse conns $ Map.insertWith (+) cave 1 seen
    edges = conns ! cave

strategy1 :: TraversalCriteria
strategy1 _ (Big _) = True
strategy1 seen cave = not $ cave `Map.member` seen

strategy2 :: TraversalCriteria
strategy2 seen Start = not $ Start `Map.member` seen
strategy2 _ End = False
strategy2 _ (Big _) = True
strategy2 seen cave = traversalCount < 2
  where
    traversalCount = fromMaybe 0 $ seen !? cave