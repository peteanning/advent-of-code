module Trim where

trim :: String -> String
trim s = f (f s)
      where f = reverse . dropWhile (== ' ')


