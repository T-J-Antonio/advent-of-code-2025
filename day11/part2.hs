module Main where
import Data.Either (fromRight)
import Text.Parsec (runParser, Parsec, char, count, sepBy, space, lower)
import Data.Map (Map, fromList, (!), notMember, delete, keys, map, filterWithKey, lookup, insert)
import Data.List (nub)
import Control.Monad.ST (runST)

-- because there are finite paths between svr and out that pass between both dac and fft,
-- there is no loop between dac and fft (that would mean infinite paths)
-- that means that either all paths are of the form:
-- svr -> dac -> fft -> out
-- or all paths are of the form:
-- svr -> fft -> dac -> out
-- thus, we can break out the search in three smaller ones, and the result is their product.

main :: IO ()
main = do
    content <- readFile "input.txt"
    let input = lines content
    let deviceGraph = fromList $ Prelude.map parseDevice input
    let fftToDac = isTherePath "dac" deviceGraph "fft"
    let (first, second) = if fftToDac then ("fft", "dac") else ("dac", "fft")
    let thirdPartPaths = findPaths "out" deviceGraph second
    let devicesToPrune = filter (/= second) . nub . concat $ thirdPartPaths
    let prunedGraph = pruneGraph second devicesToPrune deviceGraph
    let (secondPartPathsNo, cache) = pathsFrom (fromList [(second, 1)]) prunedGraph first
    let newDevicesToPrune = keys cache
    let newPrunedGraph = pruneGraph first newDevicesToPrune prunedGraph
    let (firstPartPathsNo, _) = pathsFrom (fromList [(first, 1)]) newPrunedGraph "svr"
    print (firstPartPathsNo * secondPartPathsNo * length thirdPartPaths)

parseDevice :: String -> (String, [String])
parseDevice = fromRight (error "failed to parse") . runParser device () ""

device :: Parsec String () (String, [String])
device = (,) <$> singleDevice <* char ':' <* space <*> sepBy singleDevice space

singleDevice :: Parsec String () String
singleDevice = count 3 lower

findPaths :: String -> Map String [String] -> String -> [[String]]
findPaths target _ device | target == device = [[device]]
findPaths _ graph device | notMember device graph = []
findPaths target graph device = Prelude.map (device : )
    . concatMap (findPaths target graph)
    $ graph ! device

isTherePath :: String -> Map String [String] -> String -> Bool
isTherePath _ graph device | notMember device graph = False
isTherePath target _ device | target == device = True
isTherePath target graph device = any (isTherePath target graph) $ graph ! device

pruneGraph :: String -> [String] -> Map String [String] -> Map String [String]
pruneGraph _ [] deviceGraph = deviceGraph
pruneGraph target devicesToPrune deviceGraph = pruneGraph target newDevicesToPrune valuesPruned where
    pruned = foldr delete deviceGraph devicesToPrune
    valuesPruned = Data.Map.map (filter (`notElem` devicesToPrune)) pruned
    newDevicesToPrune = keys $ filterWithKey (\k v -> null v && k /= target) valuesPruned

type Cache = Map String Int

pathsFrom :: Cache -> Map String [String] -> String -> (Int, Cache)
pathsFrom cache graph from = case Data.Map.lookup from cache of
    Just value -> (value, cache)
    Nothing -> runST $ do
        let newTos = graph ! from
        let (totalValue, updatedCache) = foldr (\newTo (accumValue, accumCache) -> (\(newValue, newCache) -> (accumValue + newValue, insert newTo newValue newCache)) $ pathsFrom accumCache graph newTo) (0, cache) newTos
        return (totalValue, updatedCache)
