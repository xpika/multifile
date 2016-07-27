import System.IO
import Data.List


main = do  
  x <- getContents
  

newLines = takeOdd.  sepBy (=='\n')

takeOdd  = map snd . filter fst . zip (cycle [True,False])

sepBy p = takeWhile (not  . null) 
          . concat
          . unfoldr 
            (Just 
             . (\(x,y)->([x,take 1 y],drop 1 y))
             . break p)
