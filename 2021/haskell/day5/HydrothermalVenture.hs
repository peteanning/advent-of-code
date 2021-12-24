module HydrothermalVenture where

import Split


type Line = (Pos,Pos)
type Pos = (Int, Int)

parseLines :: String -> [Line]
parseLines s = map (parseWords . words) $ lines s

parseWords :: [String] -> Line
parseWords [a, "->", b] = (parseTuple a, parseTuple b)

parseTuple :: String -> Pos
parseTuple v = mkTuple $ split ',' v

mkTuple :: [String] -> Pos
mkTuple [x,y] = (read x :: Int , read y :: Int)
