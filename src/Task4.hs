module Task4 (
    task4,
) where

import Data.List (isPrefixOf)

-- Horizontal Check
isHorizontalAt :: [String] -> String -> Int -> Int -> Bool
isHorizontalAt grid target m n =
    n + length target <= length (grid !! m) && target `isPrefixOf` drop n (grid !! m)

-- Vertical Check
isVerticalAt :: [String] -> String -> Int -> Int -> Bool
isVerticalAt grid target m n =
    m + length target <= length grid
        && all (\i -> grid !! (m + i) !! n == target !! i) [0 .. length target - 1]

-- Top-left to Bottom-right Diagonal Check
isDiagonalTLBRAt :: [String] -> String -> Int -> Int -> Bool
isDiagonalTLBRAt grid target m n =
    m + length target <= length grid
        && n + length target <= length (grid !! m)
        && all (\i -> grid !! (m + i) !! (n + i) == target !! i) [0 .. length target - 1]

isDiagonalBLTRAt :: [String] -> String -> Int -> Int -> Bool
isDiagonalBLTRAt grid target m n =
    m - length target + 1 >= 0
        && n + length target <= length (grid !! m) -- Ensure there are enough rows above
        && all (\i -> grid !! (m - i) !! (n + i) == target !! i) [0 .. length target - 1] -- Ensure there are enough columns to the right

checkInput :: [String] -> String -> Int -> Int -> Int
checkInput grid string m n =
    boolToInt (isHorizontalAt grid string m n)
        + boolToInt (isVerticalAt grid string m n)
        + boolToInt (isDiagonalTLBRAt grid string m n)
        + boolToInt (isDiagonalBLTRAt grid string m n)

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

checkInputXMAS :: [String] -> String -> Int -> Int -> Bool
checkInputXMAS grid string m n =
    (isDiagonalTLBRAt grid string m n || isDiagonalTLBRAt grid (reverse string) m n)
        && (isDiagonalBLTRAt grid string (m + 2) n || isDiagonalBLTRAt grid (reverse string) (m + 2) n)

task4 :: IO ()
task4 = do
    let filePath = "input/input4.txt"
    contents <- readFile filePath
    let grid = lines contents
    let y = length grid -- Number of rows
    let x = maximum (map length grid) -- Maximum length of rows
    let sumXMAS = sum [checkInput grid "XMAS" i j | i <- [0 .. y - 1], j <- [0 .. x - 1]]
    let sumSAMX = sum [checkInput grid "SAMX" i j | i <- [0 .. y - 1], j <- [0 .. x - 1]]
    -- Sum for "MAS" with boolToInt applied
    let sumMAS = sum [boolToInt $ checkInputXMAS grid "MAS" i j | i <- [0 .. y - 1], j <- [0 .. x - 1]]
    putStrLn "Task 4. Part 1"
    putStrLn $ "Sum for 'XMAS' or 'SAMX': " ++ show (sumXMAS + sumSAMX)
    putStrLn "Task 4. Part 2"
    putStrLn $ "Sum for 'MAS': " ++ show sumMAS
