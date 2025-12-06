module Main where
import Text.Parsec
import Data.Either
import Data.List

main :: IO ()
main = do
    content <- readFile "input.txt"
    let rows = lines content
    let numbersRows = map parseNumbers (init rows)
    let functions = parseOperators (last rows)
    let actualNumbers = transpose numbersRows
    let result = sum $ zipWith ($) functions actualNumbers
    print result

parseNumbers :: String -> [Int]
parseNumbers = fromRight (error "failed to parse") . runParser numbersRow () ""

parseOperators :: String -> [[Int] -> Int]
parseOperators = fromRight (error "failed to parse") . runParser operatorsRow () ""

numbersRow :: Parsec String () [Int]
numbersRow = spaces *> int `sepEndBy` spaces

operatorsRow :: Parsec String () [[Int] -> Int]
operatorsRow = spaces *> (sum <$ char '+' <|> product <$ char '*') `sepEndBy` spaces

int :: Parsec String () Int
int = read <$> many1 digit
