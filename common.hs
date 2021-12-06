module Common where

import Data.Functor
import System.IO

type FileProcessor a = [String] -> a

processDay :: String -> FileProcessor a -> IO a
processDay = readInput . getFileForDay

printDay day = processDay day print

getFileForDay :: String -> String
getFileForDay day = "inputs\\day" ++ day ++ ".txt"

readInput :: String -> FileProcessor a -> IO a
readInput name fp = withFile name ReadMode $ readWithFile fp

readWithFile :: FileProcessor a -> Handle -> IO a
readWithFile fp handle = hGetContents' handle <&> lines <&> fp

parseContent :: Read a => String -> [a]
parseContent = fmap read . lines
