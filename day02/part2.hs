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

isInvalidWithPrime :: Int -> Int -> Bool
isInvalidWithPrime prime number | length (show number) < prime = False
isInvalidWithPrime prime number = (\list -> length list == prime && all (== head list) (tail list))
    . splitInNSubdivisions prime
    . show $ number

splitInNSubdivisions :: Int -> String -> [String]
splitInNSubdivisions n str = splitInSize (div (length str) n) str

splitInSize :: Int -> String -> [String]
splitInSize n str
    | length str <= n = [str]
    | otherwise = firstPart : splitInSize n secondPart
    where (firstPart, secondPart) = splitAt n str

rangeResult :: [Range] -> Int
rangeResult = sum .
    map (\x -> if any (`isInvalidWithPrime` x) [2, 3, 5, 7] then x else 0) .
    concatMap (\(lower, upper) -> [lower..upper])
