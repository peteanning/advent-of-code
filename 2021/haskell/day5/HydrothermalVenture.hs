module HydrothermalVenture where

import Data.List (delete, sort)
import Split

import qualified Data.Set as Set
import Data.Set (empty, insert, notMember)

type Line = (Pos,Pos)
type Pos = (Int, Int)

origin = (0,0) :: Pos
zero = (origin, origin) :: Line

parseLines :: String -> [Line]
parseLines s = map (parseWords . words) $ lines s

parseWords :: [String] -> Line
parseWords [a, "->", b] = orderLine (parseTuple a, parseTuple b)
parseWords _ = zero

orderLine :: Line -> Line
orderLine (p1, p2) = if p1 > p2 then (p2, p1) else (p1, p2)

parseTuple :: String -> Pos
parseTuple v = mkTuple $ split ',' v

mkTuple :: [String] -> Pos
mkTuple [x,y] = (read x :: Int , read y :: Int)
mkTuple _ = origin

linesCover :: Line -> [Pos]
linesCover ((x0,y0), (x1, y1)) =
     linesCover' x0 y0
     where linesCover' x y | x0 == x1 && y <= y1 = (x, y) : linesCover' x (y + 1) -- x0 == x1 verical line
                           | y0 == y1 && x <= x1 = (x, y) : linesCover' (x +1) y -- x0 < x1 horizontal line
                           | otherwise = []

pointCount :: [Pos] -> Int
pointCount ps = pointCount' 0 empty (sort ps)
                where pointCount' acc _ [] = acc
                      pointCount' acc visited (p:ps) | not (null ps) =
                                                       if p == head ps && notMember p visited then
                                                         pointCount' (acc + 1) (insert p visited) ps 
                                                       else pointCount' acc visited ps
                                                     | otherwise = acc

calcVentHotSpots :: String -> Int
calcVentHotSpots filecontents = let i = parseLines filecontents
                                    covers = concatMap linesCover i
                                in
                                   pointCount covers

run :: String -> IO Int
run f = do
         contents <- readFile f
         return $ calcVentHotSpots contents



