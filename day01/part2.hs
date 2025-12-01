module Main where

main :: IO ()
main = do
    content <- readFile "input.txt"
    let result = applyAll (50, 0) . map readRotation . lines $ content
    print (snd result)
    return ()

type Rotation = (Char, Int)

readRotation :: String -> Rotation
readRotation string = (head string, read $ tail string)

-- 1st number is current position; 2nd number is number of zeroes hit
type Status = (Int, Int)

applyRotation :: Status -> Rotation -> Status
applyRotation (currentPosition, zeroes) rotation = (newPosition, zeroes + extraZeroes + roundDown + roundUp)
    where
        newPosition = changePosition rotation currentPosition
        extraZeroes = numberOfZeroes rotation currentPosition
        roundDown = if currentPosition == 0 && fst rotation == 'L' then -1 else 0
        roundUp = if newPosition == 0 && fst rotation == 'L' then 1 else 0

changePosition :: Rotation -> Int -> Int
changePosition (direction, amount) = flip mod 100 . (if direction == 'L' then subtract else (+)) amount

numberOfZeroes :: Rotation -> Int -> Int
numberOfZeroes (direction, amount) =
    abs .
    flip div 100 .
    (if direction == 'L' then subtract else (+)) amount

applyAll :: Status -> [Rotation] -> Status
applyAll = foldl applyRotation
