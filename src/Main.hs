module Main where

import System.Environment
import System.IO
import Text.XML.HaXml.XmlContent.Haskell
import Extsubset
import Data.Either

create xs = do
 files <- forM (xs) $ \x -> do
   content <- readFile x
   let q <- ( File (File_Attrs x) )
 return (show $ htmlprint $ files)  
  

main :: IO ()
main = do 
       xs <- getArgs
       case xs of 
         [] -> do
           x <- getContents    
           let p = (readXml x :: Either String Multifile)
           either print processFiles p
         '-':'c':'r':'e':'a':'t':'e':' ':xs' -> create xs' >>= putStr
         _ -> putStr "unknown usage"

processFiles (Multifile xs) = mapM_ processFile xs
processFile (File (File_Attrs path) content) = writeFile path content
