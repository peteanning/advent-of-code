module MoveCrabs where

import Data.List.Split
import Data.List (sort, group)

type Hpos = Int
-- Data.List.group . Data.List.sort . concat . map (splitOn ",") . lines <$> readFile "test.txt"
-- Data.List.sort . map (\x -> read x :: Int) . concat . map (splitOn ",") . lines <$> readFile "test.txt"
sortedPositions :: String -> [Hpos]
sortedPositions "" = []
sortedPositions s = sort . map (\x -> read x :: Hpos) . concatMap  (splitOn ",") $ lines s

frequency :: [Int] -> [(Int, Int)]
frequency n =  let grouped = group n
               in map (\(x:xs) -> (x, length (x:xs))) grouped

cheapestMove :: (Int -> [(Int, Int)] -> [(Int, Int)]) -> [(Int, Int)] -> Int
cheapestMove f fs = findCheapest $ calculateMoveCosts f (rawPositions fs) fs

calculateMoveCosts :: (Int -> [(Int, Int)] -> [(Int, Int)]) -> [Int] -> [(Int, Int)] -> [(Int, Int)]
calculateMoveCosts f (p:ps) fs = (p, totalCostOfMove (f p  fs)) : calculateMoveCosts f ps fs
calculateMoveCosts _ [] _ = []

costOfMove :: Int -> [(Int, Int)] -> [(Int, Int)]
costOfMove m = map (\(x,y) -> (x, y * abs (x - m)))

costOfMoveNonLinear :: Int -> [(Int, Int)] -> [(Int, Int)]
costOfMoveNonLinear m = let f n = sum [1..n]
                        in map (\(x,y) -> (x, y * f (abs (x - m))))

totalCostOfMove :: [(Int, Int)] -> Int
totalCostOfMove costs = sum $ map snd costs

findCheapest :: [(Int, Int)] -> Int
findCheapest cs = minimum $ map snd cs

rawPositions :: [(Int, Int)] -> [Hpos]
rawPositions fs = let positions = map fst fs
                  in [(minimum positions)..(maximum positions)]

run :: String -> IO ()
run filePath = do
                  contents <- readFile filePath
                  let result = cheapestMove (costOfMove) . frequency $ sortedPositions contents
                  let resultNonLinear = cheapestMove (costOfMoveNonLinear) . frequency $ sortedPositions contents
                  print "Linear result"
                  print result
                  print "NonLinear result"
                  print resultNonLinear
                  return () 
