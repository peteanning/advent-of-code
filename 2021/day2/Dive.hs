module Dive where

type Cmd = (String, Int)
type Pos = (Int, Int)

mkTuple :: [String] -> Cmd
mkTuple [x,y] = (x, read y :: Int)
mkTuple _ = ("undefined", 0)

getResult :: Pos -> Int
getResult (h,d) = h * d

getPos :: [Cmd] -> Pos -> Pos
getPos [] p = p
getPos (c:cs) p = getPos cs (translate c p)

translate :: Cmd -> Pos -> Pos
translate ("forward", n) (h, d) = (h + n, d)
translate ("down", n) (h, d) = (h, d + n)
translate ("up", n) (h, d) = (h, d - n)
translate (_, n) (_,_) = (-99,-99)


getWords :: FilePath -> IO ()
getWords path = do 
                   contents <- readFile path
                   let cmds = map (mkTuple . words) (lines contents) :: [Cmd] 
                   let result = getResult $ getPos cmds (0,0)
                   print result




