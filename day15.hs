{-# LANGUAGE ViewPatterns #-}

import Common (processDay)
import Data.Map as Map (Map, findMax, fromList, insert, maxViewWithKey, member, toList, (!?))
import Data.Maybe (catMaybes)
import Data.Set as Set (Set, insert, minView, singleton)
import Grid (Coordinates, asMap)

data Node
  = Point Int
  | End Int
  deriving (Show, Eq, Ord)

type RiskMap = Map Coordinates Node

type Visited = Map Coordinates TracedNode

-- This is a Set (Cost, Coordinates, Node)
-- Rather than a Map Coordinates (Cost, Node)
-- Because it allows us to quickly find the item with the lowest cost.
type Unvisited = Set PositionNode

type PositionNode = (Int, Coordinates, Node)

type TracedNode = (Int, Node)

part1 = processDay "15" $ bootstrapPath (0, 0) . insertEnd . readMap

part2 = processDay "15" $ bootstrapPath (0, 0) . insertEnd . expandMap . readMap

expandMap :: RiskMap -> RiskMap
expandMap source = Map.fromList expandedPoints
  where
    scaleFactor = 4
    expandedPoints =
      [ ((px * width + x, py * height + y), amplifyRisk (px + py) node)
        | ((x, y), node) <- Map.toList source,
          px <- [0 .. scaleFactor],
          py <- [0 .. scaleFactor]
      ]
    (width, height) = (maxX + 1, maxY + 1)
    (maxX, maxY) = fst $ Map.findMax source
    amplifyRisk amount (Point x) = Point (((amount + x - 1) `rem` 9) + 1)

readMap :: [String] -> RiskMap
readMap = asMap . fmap makeRow
  where
    makeRow = fmap (readNode . (: []))
    readNode x = Point $ read x

insertEnd :: RiskMap -> RiskMap
insertEnd (Map.maxViewWithKey -> Just ((coords, Point cost), rest)) =
  Map.insert coords (End cost) rest

bootstrapPath :: Coordinates -> RiskMap -> Maybe Int
bootstrapPath coords map = do
  node <- map !? coords
  findPath map mempty (Set.singleton (0, coords, node))

findPath :: RiskMap -> Visited -> Unvisited -> Maybe Int
findPath map visited unvisited =
  -- Look for the most promising candidate in the unvisited set
  -- (the candidate with the lowest total cost).
  case Set.minView unvisited of
    -- The unvisited set is empty, so no path can be found.
    Nothing -> Nothing
    Just ((cost, coords, node), remainingUnvisited)
      | isTarget node -> Just cost
      -- The neighbours funtion may suggest an already-evaluated node.
      -- If that's the case, just drop it from the unvisited list and retry.
      | coords `Map.member` visited -> findPath map visited remainingUnvisited
      -- We now know the best path to here, so we can place it in the visited
      -- list, and append our neighbours to the unvisited set.
      | otherwise -> findPath map visitedHere withMyNeighbours
      where
        visitedHere = Map.insert coords (cost, node) visited
        withMyNeighbours = foldr Set.insert remainingUnvisited $ neighbours map coords cost

neighbours :: RiskMap -> Coordinates -> Int -> [PositionNode]
neighbours map coords@(x, y) baseCost = catMaybes [up, down, left, right]
  where
    up = lookupNode map baseCost (x, y - 1)
    down = lookupNode map baseCost (x, y + 1)
    left = lookupNode map baseCost (x - 1, y)
    right = lookupNode map baseCost (x + 1, y)

lookupNode :: RiskMap -> Int -> Coordinates -> Maybe PositionNode
lookupNode map baseCost coords@(x, y) = do
  node <- map !? coords
  return (baseCost + costOf node, coords, node)

costOf :: Node -> Int
costOf (Point x) = x
costOf (End x) = x

isTarget :: Node -> Bool
isTarget (End _) = True
isTarget _ = False