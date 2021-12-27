module Split where

split :: Char -> String -> [String]
split d "" = []
split d xs = x : split d (drop 1 y)
             where (x,y) = span (/= d) xs
