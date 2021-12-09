module Common where

import Control.Monad (join)
import Data.Functor
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

splitBy :: (Foldable t, Eq a) => a -> t a -> [[a]]
splitBy delimiter = foldr f [[]]
  where
    f c l@(x : xs)
      | c == delimiter = [] : l
      | otherwise = (c : x) : xs