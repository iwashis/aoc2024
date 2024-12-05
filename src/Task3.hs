module Task3 (
    task3,
) where

import Text.Parsec
import Text.Parsec.String (Parser)

parseIntegers :: Parser (Integer, Integer)
parseIntegers = do
    _ <- string "mul("
    x <- many1 digit
    _ <- char ','
    y <- many1 digit
    _ <- char ')'
    return (read x, read y)

data DoDontInts = Yes | No | Pair (Integer, Integer)
    deriving (Show)

parseDoDonts :: Parser DoDontInts
parseDoDonts = try (string "do()" >> return Yes) <|> (string "don't()" >> return No)

parseDoDontsInts :: Parser [Maybe DoDontInts]
parseDoDontsInts = many tryParseOrSkip
  where
    tryParseOrSkip = try (Just . Pair <$> parseIntegers) <|> try (Just <$> parseDoDonts) <|> skipInvalid
    skipInvalid = anyChar >> returnNothing
    returnNothing = return Nothing

skipNothing :: [Maybe a] -> [a]
skipNothing [] = []
skipNothing (x : xs) = case x of
    Nothing -> skipNothing xs
    (Just y) -> y : skipNothing xs

compute :: [(Integer, Integer)] -> Integer
compute list = sum $ map (uncurry (*)) list

removeNoYesIntervals :: [DoDontInts] -> [(Integer, Integer)]
removeNoYesIntervals list = go list True
  where
    go [] _ = []
    go (x : xs) bool = case x of
        Yes -> go xs True
        No -> go xs False
        Pair p -> if bool then p : go xs bool else go xs bool

removeNoYes :: [DoDontInts] -> [(Integer, Integer)]
removeNoYes [] = []
removeNoYes (x : xs) = case x of
    Yes -> removeNoYes xs
    No -> removeNoYes xs
    Pair p -> p : removeNoYes xs

task3 :: IO ()
task3 = do
    let filePath = "input/input3.txt"
    contents <- readFile filePath
    let concatenated = concat $ lines contents
    let list = skipNothing <$> parse parseDoDontsInts "" concatenated
    let subtask1 = removeNoYes <$> list
    print "Task 3. Part 1"
    print $ compute <$> subtask1
    let subtask2 = removeNoYesIntervals <$> list
    print "Task 3. Part 2"
    print $ compute <$> subtask2
