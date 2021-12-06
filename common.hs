module Common where

import System.IO

type FileProcessor = [String] -> IO ()

processDay :: String -> FileProcessor -> IO ()
processDay = readInput . getFileForDay

printDay day = processDay day print


getFileForDay :: String -> String
getFileForDay day = "inputs\\day" ++ day ++ ".txt"

readInput :: String -> FileProcessor -> IO ()
readInput name fp = withFile name ReadMode $ readWithFile fp

readWithFile :: FileProcessor -> Handle -> IO ()
readWithFile fp handle = do
    content <- hGetContents handle
    _ <- fp $ lines content
    hClose handle
    return ()

parseContent :: Read a => String -> [a]
parseContent content = read <$> lines content
