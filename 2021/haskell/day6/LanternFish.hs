module LanternFish where
import Split ( split )
import Data.List ( group, sort )
import qualified Data.Map as Map

-- group . sort . map (\x -> read x :: Int) .  concatMap (split ',') .  lines  <$> readFile "test.txt"

loadAndRun :: String -> Int
loadAndRun fileContents = let inputNumbers = fishFromStrArray fileContents
                          in
                            0

fishFromStrArray :: String -> [[Int]]
fishFromStrArray x = group . sort . map (\y -> read y :: Int) $ concatMap (split ',') (lines x)

fishPopulation :: [[Int]] -> Map.Map Int Int
fishPopulation fish = balanceFish 0 8 $ Map.fromList (map toQuantiy fish)

balanceFish :: Int -> Int -> Map.Map Int Int -> Map.Map Int Int
balanceFish l h fish | l <= h = case Map.lookup l fish of Just f -> balanceFish (l + 1) h fish
                                                          Nothing -> balanceFish (l + 1) h (Map.insert l 0 fish)
                     | otherwise = fish

growPopulationByDays :: Int -> Map.Map Int Int -> Map.Map Int Int
growPopulationByDays n population | n > 0 = growPopulationByDays (n - 1) (growPopulation population)
                                  | otherwise = population


growPopulation :: Map.Map Int Int -> Map.Map Int Int
growPopulation fish = let agedPopulation =  balanceFish 0 8 $ Map.fromList (map decrementTimer onlyFishWithTimer)
                          onlyFishWithTimer = filter (\(n,q) -> q /= 0) (Map.toList fish)
                          spawning = agedPopulation Map.! 6
                      in
                         Map.insert 8 spawning agedPopulation

decrementTimer :: (Int, Int) -> (Int, Int)
decrementTimer (n, q) | n == 0 = (6, q)
                      | q > 0 = ((n -1), q)
                      | otherwise = (n,q)

toQuantiy :: [Int] -> (Int, Int)
toQuantiy [x] = (x, 1)
toQuantiy [] = (-1, 0)
toQuantiy (x:xs) = (x, length (x : xs))


run :: String -> IO Int
run path = do
             contents <- readFile path
             let result = loadAndRun contents
             return result


