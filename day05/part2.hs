module Main where
import Text.Parsec(Parsec, runParser, many1, digit, char, endOfLine, sepEndBy)
import Data.Either (fromRight)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let info = parseContent content
    let combined = mconcat . map newRange $ info
    let result = rangeLength combined
    print result

newtype Range = Range [Int] deriving Show

instance Semigroup Range where
    (<>) :: Range -> Range -> Range
    (Range r1) <> (Range r2) = Range $ processIntersections r1 r2

instance Monoid Range where
    mempty :: Range
    mempty = Range []
    mappend :: Range -> Range -> Range
    mappend = (<>)

processIntersections :: [Int] -> [Int] -> [Int]
processIntersections [] r = r
processIntersections r [] = r
processIntersections r1@(x : xs) r2@(y : ys)
    | x == y = if even (length r1) == even (length r2) then x : processIntersections xs ys else processIntersections xs ys
    | x < y = if even (length r2) then x : processIntersections xs r2 else processIntersections xs r2
    | otherwise = if even (length r1) then y : processIntersections r1 ys else processIntersections r1 ys

newRange :: (Int, Int) -> Range
newRange (l, u) = Range [l, u]

rangeLength :: Range -> Int
rangeLength (Range []) = 0
rangeLength (Range (x : x1 : xs)) = x1 - x + 1 + rangeLength (Range xs)

parseContent :: String -> [(Int, Int)]
parseContent = fromRight (error "failed to parse") . runParser infoParser () ""

infoParser :: Parsec String () [(Int, Int)]
infoParser = newlineSeparated range <* endOfLine <* newlineSeparated int

range :: Parsec String () (Int, Int)
range = (,) <$> int <* char '-' <*> int

int :: Parsec String () Int
int = read <$> many1 digit

newlineSeparated :: Parsec String () a -> Parsec String () [a]
newlineSeparated = flip sepEndBy endOfLine
