{-# LANGUAGE ScopedTypeVariables #-}

module Task1 (
    task1,
) where

import Data.List (sort)
import Data.Maybe (catMaybes)
import Text.Read (readMaybe)

-- Function to parse a line into a tuple of two integers
parseLine :: String -> Maybe (Int, Int)
parseLine line =
    case words line of
        [x, y] -> do
            x' <- readMaybe x
            y' <- readMaybe y
            return (x', y')
        _ -> Nothing

-- Function to read the file and parse its contents
readFileAndParse :: FilePath -> IO [(Int, Int)]
readFileAndParse filePath = do
    contents <- readFile filePath
    let linesOfFile = lines contents
        parsedLines = map parseLine linesOfFile
        validLines = catMaybes parsedLines
    return validLines

pairUp :: [(Int, Int)] -> [(Int, Int)]
pairUp list =
    let (l1, l2) = (map fst list, map snd list)
     in zip (sort l1) (sort l2)

distance :: [(Int, Int)] -> Int
distance list =
    let distances = map (\(x, y) -> abs (y - x)) $ pairUp list
     in sum distances

howOften :: [Int] -> Int -> Int
howOften list x = sum $ map (\y -> fromEnum (y == x)) list

applyHowOften :: [(Int, Int)] -> [(Int, Int)]
applyHowOften list =
    let (l1, l2) = (map fst list, map snd list)
     in let l2' = map (howOften l2) l1
         in zip l1 l2'

task2Calculate :: [(Int, Int)] -> Int
task2Calculate list = sum $ map (uncurry (*)) $ applyHowOften list

taskFromFile :: (Show b) => String -> ([(Int, Int)] -> b) -> IO ()
taskFromFile filePath d = do
    print filePath
    pairs <- readFileAndParse filePath
    let value = d pairs
    putStrLn $ "Value " ++ show value

-- Main function for demonstration
task :: (Show a) => ([(Int, Int)] -> a) -> IO ()
task f = do
    let filePaths = ["input/example_input1.txt", "input/input1.txt"] -- Replace with the path to your file
    mapM_ (`taskFromFile` f) filePaths

task1 :: IO ()
task1 = do
    print "Task 1"
    task distance
    task task2Calculate
