
import System.Process
import System.IO
import System.Directory

main = do
 dir <- getTemporaryDirectory
 (filename,handle) <- openTempFile dir "a"
 hClose handle
 writeFile filename "the rain in spain"
 system ("vim "++filename)
 x <- readFile filename
 putStrLn x
 return ()
