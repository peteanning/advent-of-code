module BinaryDiag where

data Binary = Zero | One  deriving (Eq, Show) 

toWord :: String -> [Binary]
toWord "" = [Zero]
toWord s =  map charToBin s  

charToBin :: Char -> Binary 
charToBin c = case c of '0' -> Zero
                        '1' -> One
                        _ -> undefined 

countOnes :: [[Binary]] ->  [Binary]
countOnes words = countToWord (length words) (foldl f [] words)   
          where f acc current = sumInt (map bin2Int current) acc

sumInt :: [Int] -> [Int] -> [Int]
sumInt [] ys = ys 
sumInt xs [] = xs 
sumInt (x:xs) (y:ys) = x + y : sumInt xs ys

countToWord :: Int -> [Int] -> [Binary]
countToWord l = map (\x -> if x > (l `div` 2) then One else Zero)  

bin2Int :: Binary -> Int                               
bin2Int Zero = 0 
bin2Int One = 1 

binaryArrayToInt :: [Binary] -> Int 
binaryArrayToInt [] = 0
binaryArrayToInt bs = let twoPower = 2 ^ (length bs - 1)
                      in (bin2Int (head bs) * twoPower) + binaryArrayToInt (tail bs) 

bitflip :: [Binary] -> [Binary]
bitflip = map (\b -> if b == One then Zero else One) 

runDiags :: IO ()
runDiags = do
  --contents <- readFile "test.txt"
  contents <- readFile "input.txt"
  let binaryData = map toWord (lines contents)
  let bGama = countOnes binaryData
  let gamma = binaryArrayToInt bGama
  let epsilon = binaryArrayToInt (bitflip bGama)
  let result = gamma * epsilon
  print gamma
  print epsilon
  print result



