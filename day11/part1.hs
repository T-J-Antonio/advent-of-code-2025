module Main where
import Data.Either (fromRight)
import Text.Parsec (runParser, Parsec, char, count, sepBy, space, lower)
import Data.Map (Map, fromList, (!))

main :: IO ()
main = do
    content <- readFile "input.txt"
    let input = lines content
    let deviceGraph = fromList $ map parseDevice input
    let result = countPaths deviceGraph "you"
    print result

parseDevice :: String -> (String, [String])
parseDevice = fromRight (error "failed to parse") . runParser device () ""

device :: Parsec String () (String, [String])
device = (,) <$> singleDevice <* char ':' <* space <*> sepBy singleDevice space

singleDevice :: Parsec String () String
singleDevice = count 3 lower

countPaths :: Map String [String] -> String -> Int
countPaths _ "out" = 1
countPaths graph device = sum $ map (countPaths graph) $ graph ! device
