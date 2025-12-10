module Main where
import Text.Parsec (Parsec, many1, digit, char, runParser)
import Data.Either (fromRight)
import Data.List (find, delete, sortOn)
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Control.Monad (guard)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let rows = lines content
    let tiles = map parseTile rows
    let finalState = totalMaxArea $ State (sortOn snd tiles) [] [(0, [])] 0
    print (maxArea finalState)

type Tile = (Int, Int)

parseTile :: String -> Tile
parseTile = fromRight (error "failed to parse") . runParser tile () ""

tile :: Parsec String () Tile
tile = (,) <$> int <* char ',' <*> int

int :: Parsec String () Int
int = read <$> many1 digit

areaBetween :: Tile -> Tile -> Int
areaBetween (x1, y1) (x2, y2) = (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1)

-- the input satisfies an interesting property: there are never three red tiles in a row
-- e.g. a list as follows never appears:
-- 3,6
-- 4,6
-- 7,6
-- this means that all corners and **only** corners are eligible for making rectangles 

data State = State {
    remainingTiles :: [Tile],
    eligibleTiles :: [Tile],
    intervalMap :: [(Int, [(Int, Int)])], -- fst: row, snd: intervals open in that row
    maxArea :: Int
}

totalMaxArea :: State -> State
totalMaxArea state@(State [] _ _ _) = state
totalMaxArea (State remainingTiles eligibleTiles openIntervals maxArea) =
    totalMaxArea (State newRemainingTiles newEligibleTiles allIntervals newMaxArea) where
        row = snd $ head remainingTiles
        (iterationTiles, newRemainingTiles) = span (\t -> snd t == row) remainingTiles
        newOpenIntervals = newIntervals (iterationTilesToIntervals . sortOn fst $ iterationTiles) . snd . head $ openIntervals
        allIntervals = (row, newOpenIntervals) : openIntervals
        newMaxArea = maximum $ maxArea : eligibleAreas eligibleTiles iterationTiles allIntervals
        newEligibleTiles = filter (\(x, _) -> any (\(l, h) -> h >= x && x >= l) newOpenIntervals) (eligibleTiles ++ iterationTiles)

eligibleAreas :: [Tile] -> [Tile] -> [(Int, [(Int, Int)])] -> [Int]
eligibleAreas oldTiles newTiles allIntervals = do
    t1 <- oldTiles
    t2 <- newTiles
    let (newIntervals : historic) = allIntervals
    guard (any (\(l, h) -> h >= fst t1 && fst t1 >= l) (snd . head $ historic))
    guard (all (any (\(l, h) -> h >= fst t2 && fst t2 >= l) . snd) . takeWhile ((>= snd t1) . fst) $ historic)
    return (areaBetween t1 t2)

iterationTilesToIntervals :: [Tile] -> [(Int, Int)]
iterationTilesToIntervals [] = []
iterationTilesToIntervals (t1 : t2 : ts) = (fst t1, fst t2) : iterationTilesToIntervals ts

newIntervals :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
newIntervals [] ints = ints
newIntervals (newInt : newInts) ints = newIntervals newInts (actualF ints) where
    fuseEnd = (\(l, h) -> replace (l, h) (l, snd newInt)) <$> find (\int -> fst newInt == snd int) ints
    fuseBeginning = (\(l, h) -> replace (l, h) (fst newInt, h)) <$> find (\int -> snd newInt == fst int) ints
    shrinkBeginning = (\(l, h) ->  replace (l, h) (l, fst newInt)) <$> find (\int -> snd newInt == snd int) ints
    shrinkEnd = (\(l, h) -> replace (l, h) (snd newInt, h)) <$> find (\int -> fst newInt == fst int) ints
    fuseTwo =  applyFuse <$> find (\int -> fst newInt == snd int) ints <*> find (\int -> snd newInt == fst int) ints -- fuseBeginning && fuseEnd
    end = applyEnd <$> find (== newInt) ints -- shrinkBeginning && shrinkEnd
    split = (\(l, h) -> applySplit (l, fst newInt) (snd newInt, h)) <$> find (\int -> fst newInt > fst int && snd newInt < snd int) ints
    create = (newInt :)
    actualF = fromMaybe create $ fuseTwo <|> end <|> fuseEnd <|> fuseBeginning <|> shrinkBeginning <|> shrinkEnd <|> split

applyEnd :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
applyEnd = delete

applySplit :: (Int, Int) -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
applySplit (l1, h1) (l2, h2) intervals = (l1, h1) : (l2, h2) : delete (l1, h2) intervals

replace :: (Int, Int) -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
replace old new [] = []
replace old new (int : ints)
    | int == old = new : ints
    | otherwise = int : replace old new ints

applyFuse :: (Int, Int) -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
applyFuse (l1, h1) (l2, h2) = ((l1, h2):) . delete (l2, h2) . delete (l1, h1)
