{-# LANGUAGE TupleSections #-}

import Common
import Control.Monad ((<=<))
import Data.Map (Map, delete, elems, empty, fromList, fromListWith, insertWith, toList, (!?))
import Data.Maybe (fromMaybe)

type Atom = Char

type PolymerNew = Map Pair Int

type Pair = (Atom, Atom)

type InsertionRules = Map Pair Atom

result1 = processDay "14" $ impl 10

result2 = processDay "14" $ impl 40

impl steps lines = maximum counts - minimum counts
  where
    (polymer, rules) = parseInput lines
    outcome = runSteps steps rules polymer
    counts = getAtomCounts outcome

getAtomCounts :: PolymerNew -> Map Atom Int
getAtomCounts = fmap deduplicate . fromListWith (+) . (unzipCounts <=< toList)

unzipCounts :: (Pair, Int) -> [(Atom, Int)]
unzipCounts ((l, r), count) = [(l, count), (r, count)]

-- Each atom is part of two pairs, (one with its left neighbour,
-- and one with its right neighbour) so to really count how many atoms we have,
-- we need to divide each count by two. There is one exception, however.
-- The atoms at the left and right ends of the chain are only part of one pair,
-- because they only have one neighbour. These counts are easily recognised, however,
-- because they're always odd. To fix this, we add one to odd numbers to pretend
-- they're part of a pair with a (nonexistent) outer neighbour.
deduplicate :: Int -> Int
deduplicate x
  | odd x = deduplicate $ x + 1
  | otherwise = x `div` 2

parseInput :: [String] -> (PolymerNew, InsertionRules)
parseInput (chain : _ : rules) = (polymer, fromList $ parseRule <$> rules)
  where
    polymer = fromListWith (+) pairs
    pairs = fmap (,1) $ zip chain $ tail chain
    parseRule (l : r : rest) = ((l, r), last rest)

runSteps :: Int -> InsertionRules -> PolymerNew -> PolymerNew
runSteps 0 _ atoms = atoms
runSteps n rules atoms = runSteps (n - 1) rules $ polymeriseChain rules atoms

polymeriseChain :: InsertionRules -> PolymerNew -> PolymerNew
polymeriseChain rules source = combineWith (+) unpolymerisedPairs polymerisedPairs
  where
    (unpolymerisedPairs, polymerisedPairs) = foldr applyPolymerisationRule (source, empty) $ toList rules

combineWith :: Ord k => (a -> a -> a) -> Map k a -> Map k a -> Map k a
combineWith f left right = fromListWith f $ toList right ++ toList left

applyPolymerisationRule :: (Pair, Atom) -> (PolymerNew, PolymerNew) -> (PolymerNew, PolymerNew)
applyPolymerisationRule (pair@(l, r), atom) (source, dest) = (delete pair source, appendCreatedPairs dest)
  where
    occurrences = fromMaybe 0 $ source !? pair
    appendPair k = insertWith (+) k occurrences
    appendCreatedPairs = appendPair (l, atom) . appendPair (atom, r)