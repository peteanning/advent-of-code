module Bingo where

type SetupFile = String
type Pos = Int
type Game = [(Int, Pos)]
type BingoCard = [[Int]]
type ScoredCard = [[(Int, Pos)]]

split :: Char -> String -> [String]
split d "" = []
split d xs = x : split d (drop 1 y) 
             where (x,y) = span (/= d) xs

trim :: String -> String
trim s = f (f s) 
      where f = reverse . dropWhile (== ' ')

game :: String -> [(Int,Pos)]
game "" = []
game s = zip (map read (split  ',' s) :: [Int]) [1..]


card :: [String] -> BingoCards
card (b:r1:r2:r3:r4:r5:rs) = (map ((map read . filter (/= "") . split ' ')) [r1, r2, r3, r4, r5] :: [[Int]])  : card rs
card _ = []

play :: String -> Int
play gameFile = let x = lines gameFile :: [String]
                    (gameRow,cards) = (head x, tail x) 
                    g = game gameRow
                    cs =  card  cards
                in  0

type BingoCards = [[[Int]]]
scoreGame :: BingoCards -> Game -> [ScoredCard]
scoreGame cs g = map (`score` g)  cs



score :: BingoCard -> Game -> ScoredCard
score c g = map (map (\n -> checkNumber n g)) c

checkNumber :: Int -> Game -> (Int, Int)
checkNumber n [] = (n,0) -- base case the number was not found in the game
checkNumber n ((x,p):xs) = if n == x then (n, p) else checkNumber n xs






loadsetup :: FilePath -> IO String
loadsetup file = do 
                   contents <- readFile file
                   let result = play contents
                   print result
                   return contents -- for now
              
