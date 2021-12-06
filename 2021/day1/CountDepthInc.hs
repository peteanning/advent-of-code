module CountDepthInc where

getWords :: FilePath -> IO ()
getWords path = do contents <- readFile path
                   let depths =  map read (lines contents) :: [Int]
                   let foo = reverse (zip depths [0,0..] )
                   let r = foldr  (\(accDepth, accIncCount) (currentDepth, incCount) -> if currentDepth > accDepth then (currentDepth, accIncCount + 1) else (currentDepth, accIncCount)) (0,0) foo
                   print r

