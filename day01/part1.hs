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
applyRotation (currentPosition, zeroes) rotation =
    let newPosition = changePosition rotation currentPosition
    in (newPosition, if newPosition == 0 then zeroes + 1 else zeroes)

changePosition :: Rotation -> Int -> Int
changePosition (direction, amount) = flip mod 100 . (if direction == 'L' then (+) else subtract) amount

applyAll :: Status -> [Rotation] -> Status
applyAll = foldl applyRotation
