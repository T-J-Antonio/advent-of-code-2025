module Main where
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Set (Set, fromList, singleton, size, toList, empty)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let rows = lines content
    let startingBeam = fromJust $ elemIndex 'S' (head rows)
    let result = processAllRows startingBeam (reverse $ tail rows)
    print result

type State = (Int, Set Int)

processAllRows :: Int -> [String] -> Int
processAllRows startingBeam = fst . foldr processBeams (0, singleton startingBeam)

processBeams :: String -> State -> State
processBeams row state@(prevAmount, beams) =
    foldr (addToState . processBeam row) (prevAmount, empty) (toList beams)

processBeam :: String -> Int -> Set Int
processBeam row beam = if row !! beam == '^' then fromList [beam - 1, beam + 1] else singleton beam

addToState :: Set Int -> State -> State
addToState newBeams (prevAmount, beams) = (prevAmount + (size newBeams - 1), beams <> newBeams)