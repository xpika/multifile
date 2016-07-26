import System.IO

main = do  
  x <- getContents
  


unlines = (\x -> take ((length x) `div` 2) $ map (\y -> x!!y ) [0,2..]) $ groupBy (\x y -> not $ (x/='\n') /= (y /='\n'))
