module Main where

main :: IO ()
main = do
    content <- readFile "input.txt"
    let matrix = map (map (== '@')) . lines $ content
    let result = length $ filter (isAccessible matrix) [(x, y) | x <- [0..length matrix - 1], y <- [0..length matrix - 1]] 
    print result

type Position = (Int, Int)

isAccessible :: [[Bool]] -> (Int, Int) -> Bool
isAccessible matrix (i, j)
    | not (hasRollInPosition (i, j) matrix) = False
    | otherwise = length [(x, y) | x <- [i-1..i+1], y <- [j-1..j+1], hasRollInPosition (x, y) matrix] <= 4

-- the solution assumes a square matrix (which is true for the input given)
hasRollInPosition :: Position -> [[Bool]] -> Bool
hasRollInPosition (i, j) matrix
    | i < 0 || j < 0 || i >= length matrix || j >= length matrix = False
    | otherwise = matrix !! i !! j
