module Main where
import Text.Parsec (Parsec, many1, digit, char, sepBy, space, runParser)
import Data.Foldable (Foldable(toList))

main :: IO ()
main = do
    content <- readFile "input.txt"
    let rows = lines content
    let regions = concatMap (toList . parseRegion) rows
    let validRegions = filter isValid regions
    let superValidRegions = filter isSuperValid regions
    print (length validRegions)
    print (length superValidRegions)
    -- validRegions and superValidRegions have the same length
    -- (474 for my input)
    -- thus there's no need to check for the specific layouts

type Region = (Int, Int, [Int])

parseRegion :: String -> Maybe Region
parseRegion = toMaybe . runParser region () ""

toMaybe :: Either a b -> Maybe b
toMaybe (Right x) = Just x
toMaybe _ = Nothing

region :: Parsec String () Region
region = (\(x, y) ps -> (x, y, ps)) <$> dimensions <* char ':' <* space <*> presents

dimensions :: Parsec String () (Int, Int)
dimensions = (,) <$> int <* char 'x' <*> int

presents :: Parsec String () [Int]
presents = sepBy int space

int :: Parsec String () Int
int = read <$> many1 digit

tilesNeeded :: Region -> Int
tilesNeeded (_, _, presents) =
    head presents * 7 + presents !! 1 * 7 + presents !! 2 * 5 +
    presents !! 3 * 7 + presents !! 4 * 7 + presents !! 5 * 6

tilesAvailable :: Region -> Int
tilesAvailable (x, y, _) = x * y

isValid :: Region -> Bool
isValid region = tilesAvailable region >= tilesNeeded region

generousTiles :: Region -> Int
generousTiles (_, _, presents) = sum (map (*9) presents)

isSuperValid :: Region -> Bool
isSuperValid region = tilesAvailable region >= generousTiles region
