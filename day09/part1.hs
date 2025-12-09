module Main where
import Text.Parsec (Parsec, many1, digit, char, runParser)
import Data.Either (fromRight)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let rows = lines content
    let tiles = map parseTile rows
    let result = maxArea tiles
    print result

type Tile = (Int, Int)

parseTile :: String -> Tile
parseTile = fromRight (error "failed to parse") . runParser tile () ""

tile :: Parsec String () Tile
tile = (,) <$> int <* char ',' <*> int

int :: Parsec String () Int
int = read <$> many1 digit

pairsOfTiles :: [Tile] -> [(Tile, Tile)]
pairsOfTiles [] = []
pairsOfTiles (t : ts) = map (t,) ts ++ pairsOfTiles ts

areaBetween :: Tile -> Tile -> Int
areaBetween (x1, y1) (x2, y2) = (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1)

maxArea :: [Tile] -> Int
maxArea = maximum . map (uncurry areaBetween) . pairsOfTiles
