import System.IO

main = do  
  x <- getContents
  


unlines = map snd  $ filter fst $ zip (cycle [True,False])  $ groupBy (\x y -> not $ (x/='\n') /= (y /='\n')) 
