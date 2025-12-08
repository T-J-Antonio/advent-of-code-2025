module Main where
import Data.Either (fromRight)
import Text.Parsec (runParser, many1, digit, Parsec, char)
import Data.List (sortOn, find, (\\))
import Data.Maybe (maybeToList)
import Data.Set (Set, fromList, size)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let locations = map parseLocation . lines $ content
    let result = processLocations locations
    print result

parseLocation :: String -> Location
parseLocation = fromRight (error "failed to parse") . runParser location () ""

type Location = (Int, Int, Int)

location :: Parsec String () Location
location = (,,) <$> int <* char ',' <*> int <* char ',' <*> int

int :: Parsec String () Int
int = read <$> many1 digit

distance :: Location -> Location -> Float
distance (x1, y1, z1) (x2, y2, z2) = sqrt . fromIntegral $ ((x2 - x1) ^ 2 + (y2 - y1) ^ 2 + (z2 - z1) ^ 2)

pairsOfLocations :: [Location] -> [(Location, Location)]
pairsOfLocations [] = []
pairsOfLocations (l : ls) = map (l,) ls ++ pairsOfLocations ls

data LocationInfo = LocationInfo {
    point1 :: Location,
    point2 :: Location,
    distanceBetween :: Float
}

pairToInfo :: (Location, Location) -> LocationInfo
pairToInfo (l1, l2) = LocationInfo l1 l2 (distance l1 l2)

allDistances :: [Location] -> [LocationInfo]
allDistances = map pairToInfo . pairsOfLocations

makeCircuits :: Int -> [LocationInfo] -> LocationInfo
makeCircuits = makeCircuitsAux []

makeCircuitsAux :: [Set Location] -> Int -> [LocationInfo] -> LocationInfo
makeCircuitsAux circuits target (l : ls) = if cutCondition
    then l
    else makeCircuitsAux totalCircuits target ls
    where
        circuitsConnected =
            maybeToList (find (elem (point1 l)) circuits) ++ maybeToList (find (elem (point2 l)) circuits)
        newCircuits = case circuitsConnected of
            [] -> [fromList [point1 l, point2 l]]
            [circuit] -> [circuit <> fromList [point1 l, point2 l]]
            [circuit1, circuit2] -> [circuit1 <> circuit2]
        totalCircuits = (circuits \\ circuitsConnected) ++ newCircuits
        cutCondition = length totalCircuits == 1 && size (head totalCircuits) == target

processLocations :: [Location] -> Int
processLocations locations = (\(LocationInfo (x1, _, _) (x2, _, _) _) -> x1 * x2)
    . makeCircuits (length locations)
    . sortOn distanceBetween
    . allDistances $ locations
