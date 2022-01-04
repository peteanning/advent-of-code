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
cheapestMove f fs = findCheapest $ calculateMoveCosts f (range fs) fs

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

range :: [(Int, Int)] -> [Hpos]
range fs = let positions = frequencyUnMap fs 
               m = mean positions
               v = variance positions
                  in [(m - v)..(m + v)]

frequencyUnMap :: [(Int, Int)] -> [Int]
frequencyUnMap fs = concatMap (\(x,y) -> take y (cycle [x])) fs


mean :: [Int] -> Int
mean ns = sum ns `div` length ns

median :: [Int] -> Int
median ns = let l = length ns
                mid = l `div` 2
            in
              if odd l then ns !! mid else mean $ (ns !! mid) : [ns !! (mid - 1)]

variance :: [Int] -> Int
variance ns = let m = mean ns
              in sum (map (\n -> (n - m)^2) ns) `div` length ns

sdev :: [Int] -> Int
sdev ns = let sd = sqrt $ fromIntegral (variance ns)
          in round sd

run :: String -> IO ()
run filePath = do
                  contents <- readFile filePath
                  let result = cheapestMove costOfMove . frequency $ sortedPositions contents
                  let resultNonLinear = cheapestMove costOfMoveNonLinear . frequency $ sortedPositions contents
                  print "Linear result"
                  print result
                  print "NonLinear result"
                  print resultNonLinear
                  return ()
