{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Bingo where

import Data.List (find, transpose)
import qualified Data.Maybe

type SetupFile = String
type Pos = Int
type Game = [(Int, Pos)]
type BingoCard = [[Int]]
type ScoredCard = [[(Int, Pos)]]

split :: Char -> String -> [String]
split d "" = []
split d xs = x : split d (drop 1 y)
             where (x,y) = span (/= d) xs

game :: String -> [(Int,Pos)]
game "" = []
game s = zip (map read (split  ',' s) :: [Int]) [1..]

card :: [String] -> BingoCards
card (b:r1:r2:r3:r4:r5:rs) = (map (map read . filter (/= "") . split ' ') [r1, r2, r3, r4, r5] :: [[Int]])  : card rs
card _ = []

-- the main function that plays the game
play :: SetupFile -> Int
play gameFile = let x = lines gameFile :: [String]
                    (gameRow,cards) = (head x, tail x)
                    g = game gameRow
                    bingoCards = card  cards -- parse all the cards
                    _play drawNumber = if any isComplete sg then
                                          scoreWinner (findWinner sg) (g !! (drawNumber -1))
                                      --    findWinner sg
                                       else
                                         _play (drawNumber + 1)
                                       where
                                          sg = scoreGame bingoCards (take drawNumber g)
                   in  _play 5 -- remember we can only win with 5 or more numbers

-- sum of all unmarked numbers * the last number drawn
scoreWinner :: ScoredCard -> (Int, Pos) -> Int
scoreWinner scoredCard (n,p) = n * sum (map (\(x,y) -> if y == 0 then x else 0) (concat scoredCard))

-- take all the cards from all the players and score them with a game
type BingoCards = [[[Int]]]
scoreGame :: BingoCards -> Game -> [ScoredCard]
scoreGame cs g = map (`score` g)  cs

findWinner :: [ScoredCard] -> ScoredCard
findWinner cards = Data.Maybe.fromMaybe [] (find isComplete cards)

isComplete :: ScoredCard -> Bool
isComplete sc = any isCompleteLine sc || any isCompleteLine (transpose sc)

isCompleteLine :: [(Int,Pos)] -> Bool
isCompleteLine ((v,p):xs) = p /= 0 && isCompleteLine xs
isCompleteLine [] = True

-- take a card and a Game and score the card
score :: BingoCard -> Game -> ScoredCard
score c g = map (map (`checkNumber` g)) c

checkNumber :: Int -> Game -> (Int, Int)
checkNumber n [] = (n,0) -- base case the number was not found in the game
checkNumber n ((x,p):xs) = if n == x then (n, p) else checkNumber n xs


loadsetup :: FilePath -> IO SetupFile
loadsetup file = do
                   contents <- readFile file
                   let result = play contents
                   print result
                   return contents -- for now

