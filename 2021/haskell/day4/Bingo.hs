module Bingo where

import Data.List (find, transpose)
import qualified Data.Maybe

type SetupFile = String
type Pos = Int
type Draw = [(Int, Pos)]
type Board = [[Int]]
type ScoredBoard = [[(Int, Pos)]]

split :: Char -> String -> [String]
split d "" = []
split d xs = x : split d (drop 1 y)
             where (x,y) = span (/= d) xs

parseDraw :: String -> Draw
parseDraw "" = []
parseDraw s = zip (map read (split  ',' s) :: [Int]) [1..]

parseBoards :: [String] -> Boards
parseBoards (b:r1:r2:r3:r4:r5:rs) = (map (map read . filter (/= "") . split ' ') [r1, r2, r3, r4, r5] :: [[Int]])  : parseBoards rs
parseBoards _ = []

-- the main function that plays the game
play :: SetupFile -> Int
play gameFile = let x = lines gameFile :: [String]
                    (gameRow, boards) = (head x, tail x)
                    g = parseDraw gameRow
                    bingoBoards = parseBoards  boards 
                    _play drawNumber = if any isComplete sg then
                                          scoreWinner (findWinner sg) (g !! (drawNumber -1))
                                      --    findWinner sg
                                       else
                                         _play (drawNumber + 1)
                                       where
                                          sg = scoreDraw bingoBoards (take drawNumber g)
                   in  _play 5 -- remember we can only win with 5 or more numbers

-- sum of all unmarked numbers * the last number drawn
scoreWinner :: ScoredBoard -> (Int, Pos) -> Int
scoreWinner scoredBoards (n,p) = n * sum (map (\(x,y) -> if y == 0 then x else 0) (concat scoredBoards))

-- take all the Boards from all the players and score them with a game
type Boards = [[[Int]]]
scoreDraw :: Boards -> Draw -> [ScoredBoard]
scoreDraw cs g = map (`score` g)  cs

findWinner :: [ScoredBoard] -> ScoredBoard
findWinner boards = Data.Maybe.fromMaybe [] (find isComplete boards)

isComplete :: ScoredBoard -> Bool
isComplete sc = any isCompleteLine sc || any isCompleteLine (transpose sc)

isCompleteLine :: [(Int,Pos)] -> Bool
isCompleteLine ((v,p):xs) = p /= 0 && isCompleteLine xs
isCompleteLine [] = True

-- take a boards and a Draw and score the boards
score :: Board -> Draw -> ScoredBoard
score c g = map (map (`checkNumber` g)) c

checkNumber :: Int -> Draw -> (Int, Int)
checkNumber n [] = (n,0) -- base case the number was not found in the game
checkNumber n ((x,p):xs) = if n == x then (n, p) else checkNumber n xs


loadsetup :: FilePath -> IO SetupFile
loadsetup file = do
                   contents <- readFile file
                   let result = play contents
                   print result
                   return contents -- for now

