module Main where
import Data.List (find)
import Data.Maybe (fromJust, maybeToList, isJust)
import Data.Map (Map, empty, insertWith, findWithDefault, lookup, insert)
import Control.Monad.ST (runST)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let rows = lines content
    let (starting : nodeList) = nodes rows
    let result = processFromNode (optimizeNodes nodeList) empty starting
    print (fst result)

nodes :: [String] -> [(Int, Int)]
nodes = map (\(i, j, _) -> (i, j))
    . filter (\(_, _, c) -> c == '^')
    . concatMap (\(i, row) -> map (\(j, c) -> (i, j, c)) row)
    . zip [0..]
    . map (zip [0..])

-- key: column; value: nodes in that column
type ColumnMap = Map Int [Int]

optimizeNodes :: [(Int, Int)] -> ColumnMap
optimizeNodes = foldr insertNodeInMap empty

insertNodeInMap :: (Int, Int) -> ColumnMap -> ColumnMap
insertNodeInMap (i, j) = insertWith (++) j [i]

nextNodes :: ColumnMap -> (Int, Int) -> [(Int, Int)]
nextNodes columnMap (i, j) = maybeToList left ++ maybeToList right
    where
        left = nextNode columnMap (j - 1) i
        right = nextNode columnMap (j + 1) i

nextNode :: ColumnMap -> Int -> Int -> Maybe (Int, Int)
nextNode columnMap column startingRow =
    (, column) <$> find (> startingRow) (findWithDefault [] column columnMap)

-- key: node, value: number of children
type Cache = Map (Int, Int) Int

processFromNode :: ColumnMap -> Cache -> (Int, Int) -> (Int, Cache)
processFromNode columnMap cache node = (firstResult + secondResult, superUpdatedCache)
    where
        firstNode = nextNodes columnMap node !? 0
        (firstResult, updatedCache) = case firstNode of
            Nothing -> (1, cache)
            Just aNode -> runST $ do
                let cacheHit = Data.Map.lookup aNode cache
                let (result, newCache) = if isJust cacheHit
                    then (fromJust cacheHit, cache)
                    else processFromNode columnMap cache aNode
                let actualNewCache = if isJust cacheHit then newCache else insert aNode result newCache
                return (result, actualNewCache)
        secondNode = nextNodes columnMap node !? 1
        (secondResult, superUpdatedCache) = case secondNode of
            Nothing -> (1, updatedCache)
            Just aNode -> processFromNode columnMap updatedCache aNode

(!?) :: [a] -> Int -> Maybe a
[] !? _ = Nothing
(x : xs) !? 0 = Just x
(x : xs) !? n = xs !? (n - 1)
