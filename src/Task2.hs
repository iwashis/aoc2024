module Task2 (
    task2,
) where

import Data.Either (rights)
import Text.Parsec
import Text.Parsec.String (Parser)

parseInteger :: Parser Int
parseInteger = do
    x <- many1 digit
    _ <- optional $ char ' '
    return $ read x

parseIntegers :: Parser [Int]
parseIntegers = many parseInteger

neigboursSatisfy :: (Int -> Int -> Bool) -> [Int] -> Bool
neigboursSatisfy f list = go list Nothing
  where
    go [] _ = True
    go (x : xs) Nothing = go xs (Just x)
    go (y : xs) (Just x) = f x y && go xs (Just y)

differBy :: (Int -> Int -> Bool)
differBy x y = abs (y - x) >= 1 && abs (y - x) <= 3

extractEach :: [a] -> [[a]]
extractEach xs = [take i xs ++ drop (i + 1) xs | i <- [0 .. length xs - 1]]

checkConditions :: [Int] -> Bool
checkConditions xs =
    (neigboursSatisfy (<) xs || neigboursSatisfy (>) xs) && neigboursSatisfy differBy xs
checkPart2Conditions :: [Int] -> Bool
checkPart2Conditions list =
    checkConditions list || any checkConditions (extractEach list)

countTrues :: [Bool] -> Int
countTrues = length . filter id
task2 :: IO ()
task2 = do
    let filePath = "input/input2.txt"
    contents <- readFile filePath
    let linesOfFile = lines contents
    let parsed = rights (fmap (parse parseIntegers "") linesOfFile)
    let isGood = fmap checkConditions parsed
    let isGood2 = fmap checkPart2Conditions parsed
    print "Task 2. Part 1."
    print $ countTrues isGood
    print "Task 2. Part 2."
    print $ countTrues isGood2
