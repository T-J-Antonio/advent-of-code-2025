module Main where
import Text.Parsec(Parsec, runParser, many1, digit, char, endOfLine, sepEndBy)
import Data.Either (fromRight)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let info = parseContent content
    let result = spoiledIngredients info
    print result

data Info = Info {
    freshRanges :: [(Int, Int)],
    ingredients :: [Int]
} deriving Show

parseContent :: String -> Info
parseContent = fromRight (error "failed to parse") . runParser infoParser () ""

infoParser :: Parsec String () Info
infoParser = Info <$> newlineSeparated range <* endOfLine <*> newlineSeparated int

range :: Parsec String () (Int, Int)
range = (,) <$> int <* char '-' <*> int

int :: Parsec String () Int
int = read <$> many1 digit

newlineSeparated :: Parsec String () a -> Parsec String () [a]
newlineSeparated = flip sepEndBy endOfLine

spoiledIngredients :: Info -> Int
spoiledIngredients (Info freshRanges ingredients) = length $ filter (\i -> any (($ i) . isInRange) freshRanges) ingredients

isInRange :: (Int, Int) -> Int -> Bool
isInRange (lower, upper) n = n >= lower && n <= upper
