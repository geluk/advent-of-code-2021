module Common where

import Control.Monad (join)
import Data.Functor ((<&>))
import Data.Text (pack, replace, splitOn, unpack)
import System.IO

type FileProcessor a = [String] -> a

processDay :: String -> FileProcessor a -> IO a
processDay = readInput . getFileForDay

printDay day = join $ processDay day print

getFileForDay :: String -> String
getFileForDay day = "inputs\\day" ++ day ++ ".txt"

readInput :: String -> FileProcessor a -> IO a
readInput name fp = withFile name ReadMode $ readWithFile fp

readWithFile :: FileProcessor a -> Handle -> IO a
readWithFile fp handle = hGetContents' handle <&> lines <&> fp

parseContent :: Read a => String -> [a]
parseContent = fmap read . lines

-- String functions --
-- ================ --
replaceString :: String -> String -> String -> String
replaceString needle replacement = unpack . replace (pack needle) (pack replacement) . pack

splitOn :: String -> String -> [String]
splitOn needle = fmap unpack . Data.Text.splitOn (pack needle) . pack

splitBy :: (Foldable t, Eq a) => a -> t a -> [[a]]
splitBy delimiter = foldr f [[]]
  where
    f c l@(x : xs)
      | c == delimiter = [] : l
      | otherwise = (c : x) : xs

-- List functions --
-- ============== --
withIndex :: [a] -> [(Int, a)]
withIndex = reverse . foldl f []
  where
    f [] x = [(0, x)]
    f ((pi, px) : ts) x = (pi + 1, x) : (pi, px) : ts

-- Taken from Data.List.Extra
(!?) :: [a] -> Int -> Maybe a
xs !? n
  | n < 0 = Nothing
  | otherwise =
    foldr f (const Nothing) xs n
  where
    f x r k = case k of
      0 -> Just x
      _ -> r (k - 1)