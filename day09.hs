import Common (processDay)
import Data.List (sort)
import Data.Maybe (catMaybes, mapMaybe)
import Grid

type Height = Int

type HeightMap = Grid Height

result1 = processDay "09" (calcRiskLevel . findLowPoints . readMap)

readMap :: [String] -> HeightMap
readMap = fmap makeRow
  where
    makeRow = fmap (read . (: []))

calcRiskLevel :: [Int] -> Int
calcRiskLevel = sum . (riskLevel <$>)

findLowPoints :: HeightMap -> [Int]
findLowPoints map = (mapMaybe dropLow . asCoordinateList) map
  where
    dropLow (coords, v)
      | isLowPoint map coords = Just v
      | otherwise = Nothing

isLowPoint :: HeightMap -> Coordinates -> Bool
isLowPoint grid coords@(x, y) = centre < lowestNeighbour
  where
    centre = grid !!! coords
    lowestNeighbour = (minimum . catMaybes) [above, below, left, right]
    above = grid !!? (x, y - 1)
    below = grid !!? (x, y + 1)
    left = grid !!? (x - 1, y)
    right = grid !!? (x + 1, y)

riskLevel :: Int -> Int
riskLevel = (+ 1)
