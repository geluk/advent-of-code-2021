import Common (processDay)
import Data.Map (Map, elems, insert, keys, (!), (!?))
import Data.Maybe (fromMaybe)
import Grid (Coordinates, asMap)
import System.Posix.Internals (CLconv)

type Octopus = Int

type OctopusMap = Map Coordinates Octopus

result1 = processDay "11" $ runIteration 100 . readMap

result2 = processDay "11" $ findSynchronisationPoint 1 . readMap

readMap :: [String] -> OctopusMap
readMap = asMap . fmap makeRow
  where
    makeRow = fmap (read . (: []))

runIteration :: Int -> OctopusMap -> Int
runIteration 0 map = 0
runIteration x map = countFlashes nextMap + runIteration (x - 1) (resetMap nextMap)
  where
    countFlashes = length . filter (> 9) . elems
    nextMap = updateMap map

findSynchronisationPoint :: Int -> OctopusMap -> Int
findSynchronisationPoint iteration map
  | all (== 0) map = iteration - 1
  | otherwise = findSynchronisationPoint (iteration + 1) . resetMap . updateMap $ map

allFlashed :: OctopusMap -> Bool
allFlashed = all (> 9)

resetMap :: OctopusMap -> OctopusMap
resetMap = (reset <$>)
  where
    reset 10 = 0
    reset x = x

updateMap :: OctopusMap -> OctopusMap
updateMap map = foldr tryBoostOctopusAt map $ keys map

tryBoostOctopusAt :: Coordinates -> OctopusMap -> OctopusMap
tryBoostOctopusAt coords map = fromMaybe map newMap
  where
    octopus = map !? coords
    newMap = fmap (boostOctopusAt coords map) octopus

boostOctopusAt :: Coordinates -> OctopusMap -> Octopus -> OctopusMap
boostOctopusAt coords map octopus =
  if flashed
    then boostNeighbours coords . insert coords newOctopus $ map
    else insert coords newOctopus map
  where
    (newOctopus, flashed) = boostOctopus octopus

-- Yo dawg, I heard you like function composition
boostNeighbours :: Coordinates -> OctopusMap -> OctopusMap
boostNeighbours = foldr1 (.) . fmap tryBoostOctopusAt . surroundingCoordinates

boostOctopus :: Octopus -> (Octopus, Bool)
boostOctopus 9 = (10, True)
boostOctopus 10 = (10, False)
boostOctopus x = (x + 1, False)

surroundingCoordinates :: Coordinates -> [Coordinates]
surroundingCoordinates (x, y) =
  [ (nx, ny)
    | nx <- [x - 1 .. x + 1],
      ny <- [y - 1 .. y + 1],
      nx /= x || ny /= y
  ]