module LanternFish where
import Split ( split )
import Data.List ( group, sort )
import Data.Maybe ( fromMaybe )
import qualified Data.Map as Map
import System.Posix (BaudRate(B4800))

-- group . sort . map (\x -> read x :: Int) .  concatMap (split ',') .  lines  <$> readFile "test.txt"

loadAndRun :: String -> Int -> Int
loadAndRun fileContents  days = let inputNumbers = fishFromStrArray fileContents
                                    population = fishPopulation inputNumbers
                                    grown = growPopulationByDays days population
                          in
                            countPopulation (Map.toList grown)

fishFromStrArray :: String -> [[Int]]
fishFromStrArray x = group . sort . map (\y -> read y :: Int) $ concatMap (split ',') (lines x)

fishPopulation :: [[Int]] -> Map.Map Int Int
fishPopulation fish = balanceFish 0 8 $ Map.fromList (map toQuantiy fish)

balanceFish :: Int -> Int -> Map.Map Int Int -> Map.Map Int Int
balanceFish l h fish | l <= h = case Map.lookup l fish of Just f -> balanceFish (l + 1) h fish
                                                          Nothing -> balanceFish (l + 1) h (Map.insert l 0 fish)
                     | otherwise = fish

countPopulation :: [(Int, Int)] -> Int 
countPopulation [] = 0 
countPopulation ((f,q):fs) = q + countPopulation fs



growPopulationByDays :: Int -> Map.Map Int Int -> Map.Map Int Int
growPopulationByDays n population | n > 0 = growPopulationByDays (n - 1) (growPopulation population)
                                  | otherwise = population


growPopulation :: Map.Map Int Int -> Map.Map Int Int
growPopulation fish = let agedPopulation =  balanceFish 0 8 $ Map.fromList (map decrementTimer onlyFishWithTimer)
                          onlyFishWithTimer = sort $ filter (\(n,q) -> q /= 0) (Map.toList fish)
                      in
                        spawn agedPopulation


decrementTimer :: (Int, Int) -> (Int, Int)
decrementTimer (n, q)  = (n -1, q)

toQuantiy :: [Int] -> (Int, Int)
toQuantiy [x] = (x, 1)
toQuantiy [] = (-1, 0)
toQuantiy (x:xs) = (x, length (x : xs))


newBorn :: Map.Map Int Int -> Int 
newBorn fish = fromMaybe 0 (Map.lookup (-1) fish) 

spawn :: Map.Map Int Int -> Map.Map Int Int
spawn fish = let nb =  newBorn fish
                 cleanFish = Map.delete (-1) fish 
                 sixes = (fromMaybe 0 (Map.lookup 6 cleanFish)) + nb
             in
               -- we should never get fish with a timer of 8
               Map.insert 8 nb (Map.adjust (const sixes) 6 cleanFish)


run :: String -> Int -> IO Int
run path days = do
             contents <- readFile path
             let result = loadAndRun contents days
             return result


