module Main where
import Text.Parsec (Parsec, many1, digit, char, (<|>), sepEndBy1, sepBy1, runParser)
import Data.Either (fromRight, isLeft)
import Data.Set (Set, fromList, toList, singleton)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let rows = lines content
    let machines = map parseRow rows
    let result = sum $ map processMachine machines
    print result

data Machine = Machine {
    requirements :: [Bool],
    buttons :: [[Int]]
}

parseRow :: String ->  Machine
parseRow = fromRight (error "failed to parse") . runParser parseMachine () ""

parseMachine :: Parsec String () Machine
parseMachine = Machine <$> parseRequirements <* char ' ' <*> parseButtons <* parseJoltages

parseJoltages :: Parsec String () [Int]
parseJoltages = char '{' *> sepBy1 int (char ',') <* char '}'

parseRequirements :: Parsec String () [Bool]
parseRequirements = char '[' *> many1 ((False <$ char '.') <|> (True <$ char '#')) <* char ']'

parseButtons :: Parsec String () [[Int]]
parseButtons = sepEndBy1 button (char ' ')

button :: Parsec String () [Int]
button = char '(' *> sepBy1 int (char ',') <* char ')'

int :: Parsec String () Int
int = read <$> many1 digit

pressButton :: [Int] -> [Bool] -> [Bool]
pressButton xs lights
  = foldr (changeAt not) lights xs

changeAt :: (a -> a) -> Int -> [a] -> [a]
changeAt _ _ [] = []
changeAt f 0 (x : xs) = f x : xs
changeAt f n (x : xs) = x : changeAt f (n - 1) xs

processMachine :: Machine -> Int
processMachine machine = processRow machine (singleton (startingLights machine))

startingLights :: Machine -> [Bool]
startingLights = map (const False) . requirements

processRow :: Machine -> Set [Bool] -> Int
processRow machine lightCombinations | requirements machine `elem` lightCombinations = 0
processRow machine lightCombinations = 1 + processRow machine (fromList newCombinations) where
    lightList = toList lightCombinations
    newCombinations = [newLights |
        lights <- lightList,
        button <- buttons machine,
        let newLights = pressButton button lights]
