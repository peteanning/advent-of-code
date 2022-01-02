module CountDepthInc  where

getIncreases :: [Int] -> Int -> Int
getIncreases [] _ = 0
getIncreases [n] _ = 0
getIncreases (n:ns) acc = if n < head (ns) then 
                            getIncreases ns acc + 1 else 
                            getIncreases ns acc



main :: FilePath -> IO ()
main path = do contents <- readFile path
               let depths =  map read (lines contents) :: [Int]
               print $ getIncreases depths 0
               return ()
 
