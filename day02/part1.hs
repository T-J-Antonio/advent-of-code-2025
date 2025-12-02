module Main where
import Text.Parsec (char, runParser, Parsec, sepBy, digit, many1)
import Data.Either (fromRight)

main = do
    content <- readFile "input.txt"
    let ranges = parseContent content
    let result = rangeResult ranges
    print result
    return ()

type Range = (Int, Int)

parseContent :: String -> [Range]
parseContent = fromRight (error "failed to parse") . runParser (commaSeparated range) () ""

commaSeparated :: Parsec String () a -> Parsec String () [a]
commaSeparated = flip sepBy (char ',')

range :: Parsec String () Range
range = (\x _ y -> (read x, read y)) <$> many1 digit <*> char '-' <*> many1 digit

isInvalid :: Int -> Bool
isInvalid id = (\(x, y) -> x == y) $ splitAt (div (length . show $ id) 2) (show id)

rangeResult :: [Range] -> Int
rangeResult = sum .
    map (\x -> if isInvalid x then x else 0) .
    concatMap (\(lower, upper) -> [lower..upper])
