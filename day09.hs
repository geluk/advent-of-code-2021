import Common (processDay)
import Data.List (sort)
import Data.Map (Map, insert, keys, toList, (!), (!?))
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Grid

type HeightMap = Map Coordinates Height

type Height = Int

type BasinMap = Map Coordinates BasinTile

data BasinTile
  = Unvisited Height
  | Visited
  deriving (Show)

result1 = processDay "09" (calcRiskLevel . fmap snd . findLowPoints . readMap)

result2 = processDay "09" (product . take 3 . reverse . sort . findBasinSizes . toBasinMap . readMap)

toBasinMap :: HeightMap -> BasinMap
toBasinMap = fmap Unvisited

findBasinSizes :: BasinMap -> [Int]
findBasinSizes map = snd $ foldl findBasins (map, []) $ keys map

findBasins :: (BasinMap, [Int]) -> Coordinates -> (BasinMap, [Int])
findBasins (map, sizes) coords
  | foundSize == 0 = (newMap, sizes)
  | otherwise = (newMap, foundSize : sizes)
  where
    (newMap, foundSize) = traverseBasin coords (map, 0)

traverseBasin :: Coordinates -> (BasinMap, Int) -> (BasinMap, Int)
traverseBasin coords@(x, y) (map, size) =
  if maybe False isValidTile currentTile
    then (left . down . right . up) (visit coords map, size + 1)
    else (map, size)
  where
    currentTile = map !? coords
    up = traverseBasin (x, y - 1)
    right = traverseBasin (x + 1, y)
    down = traverseBasin (x, y + 1)
    left = traverseBasin (x - 1, y)

visit :: Coordinates -> BasinMap -> BasinMap
visit coords = insert coords Visited

isValidTile :: BasinTile -> Bool
isValidTile Visited = False
isValidTile (Unvisited 9) = False
isValidTile _ = True

readMap :: [String] -> HeightMap
readMap = asMap . fmap makeRow
  where
    makeRow = fmap (read . (: []))

findLowPoints :: HeightMap -> [(Coordinates, Height)]
findLowPoints map = (mapMaybe dropLow . toList) map
  where
    dropLow (coords, v)
      | isLowPoint map coords = Just (coords, v)
      | otherwise = Nothing

isLowPoint :: HeightMap -> Coordinates -> Bool
isLowPoint grid coords@(x, y) = centre < lowestNeighbour
  where
    centre = grid ! coords
    lowestNeighbour = (minimum . catMaybes) [above, below, left, right]
    above = grid !? (x, y - 1)
    below = grid !? (x, y + 1)
    left = grid !? (x - 1, y)
    right = grid !? (x + 1, y)

calcRiskLevel :: [Height] -> Int
calcRiskLevel = sum . (riskLevel <$>)

riskLevel :: Int -> Int
riskLevel = (+ 1)
