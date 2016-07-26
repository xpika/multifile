import System.IO

main = do  
  x <- getContents
  


unlines = takeWhile (/="") $ unfoldr (Just .(\(x,y)->(x, dropWhile (=='\n') y)) . span (/= '\n'))
