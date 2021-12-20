module Bingo where
import Data.List (dropWhileEnd)

type BingoCard = [[(Int, Bool)]]
type SetupFile = String
type Game = [Int]


split :: Char -> String -> [String]
split d "" = []
split d xs = x : split d (drop 1 y) 
             where (x,y) = span (/= d) xs

trim :: String -> String
trim s = f (f s) 
      where f = reverse . dropWhile (== ' ')
            
game :: String -> [Int]
game "" = []
game s = map read (split  ',' s) :: [Int]

card :: [String] -> [[[Int]]]
card (b:r1:r2:r3:r4:r5:rs) = (map ((map read . filter (/= "") . split ' ')) [r1, r2, r3, r4, r5] :: [[Int]])  : card rs
card _ = []


loadsetup :: FilePath -> IO String
loadsetup file = do 
                   contents <- readFile file
                   let x = lines contents :: [String]
                   let (gameRow,cards) = (head x, tail x) 
                   let g = game gameRow
                   let cs =  card  cards
                   --print g
                   --print cards
                   print cs
                   return contents -- for now
              
