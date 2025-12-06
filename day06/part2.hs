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
    let firstIterationNs = transpose numbersRows
    let widths = map maxDigits firstIterationNs
    let secondIterationNs = transpose $ map (parseWithFixedWidths widths) (init rows)
    let cephalopodNs = map (map read . transpose) secondIterationNs
    let result = sum $ zipWith ($) functions cephalopodNs
    print result

maxDigits :: [String] -> Int
maxDigits = maximum . map length

parseWithFixedWidths :: [Int] -> String -> [String]
parseWithFixedWidths [] "" = []
parseWithFixedWidths (w : ws) str = take w str : parseWithFixedWidths ws (drop (w + 1) str)

parseNumbers :: String -> [String]
parseNumbers = fromRight (error "failed to parse") . runParser numbersRow () ""

parseOperators :: String -> [[Int] -> Int]
parseOperators = fromRight (error "failed to parse") . runParser operatorsRow () ""

numbersRow :: Parsec String () [String]
numbersRow = many1 number

operatorsRow :: Parsec String () [[Int] -> Int]
operatorsRow = spaces *> (sum <$ char '+' <|> product <$ char '*') `sepEndBy` spaces

number :: Parsec String () String
number = (\_ n _ -> n) <$> many space <*> many1 digit <*> many space
