module Main where

main :: IO ()
main = do
    content <- readFile "input.txt"
    let matrix = map (map (== '@')) . lines $ content
    let result = recursivelyRemoveRolls matrix
    print result

type Position = (Int, Int)
type Matrix = [[Bool]]

isAccessible :: Matrix -> Position -> Bool
isAccessible matrix (i, j)
    | not (hasRollInPosition (i, j) matrix) = False
    | otherwise = length [(x, y) | x <- [i-1..i+1], y <- [j-1..j+1], hasRollInPosition (x, y) matrix] <= 4

-- the solution assumes a square matrix (which is true for the input given)
hasRollInPosition :: Position -> Matrix -> Bool
hasRollInPosition (i, j) matrix
    | i < 0 || j < 0 || i >= length matrix || j >= length matrix = False
    | otherwise = matrix !! i !! j

removeAllAccessibles :: [Position] -> Matrix -> Matrix
removeAllAccessibles [] matrix = matrix
removeAllAccessibles ((i, j) : rest) matrix = removeAllAccessibles rest $
    take i matrix ++
    [take j (matrix !! i) ++ [False] ++ drop (j + 1) (matrix !! i)] ++
    drop (i + 1) matrix

accessiblePositions :: Matrix -> [Position]
accessiblePositions matrix = filter (isAccessible matrix) [(x, y) | x <- [0..length matrix - 1], y <- [0..length matrix - 1]]

recursivelyRemoveRolls :: Matrix -> Int
recursivelyRemoveRolls matrix
    | null rollsToRemove = 0
    | otherwise = length rollsToRemove + recursivelyRemoveRolls newMatrix
    where
        rollsToRemove = accessiblePositions matrix
        newMatrix = removeAllAccessibles rollsToRemove matrix
