module Main where

import System.Environment
import System.IO
import Text.XML.HaXml.XmlContent.Haskell
import Extsubset
import Data.Either

create xs = do
 files <- forM xs $ \filePath -> do
   content <- readFile filePath
   return ( File (File_Attrs filePath) content )
 return (show $ htmlprint $ files)

main :: IO ()
main = do 
       xs <- getArgs
       case xs of 
         [] -> do
           x <- getContents    
           let p = (readXml x :: Either String Multifile)
           either print processFiles p
         "-create":xs' -> create xs' >>= putStr
         _ -> putStr "unknown usage"

processFiles (Multifile xs) = mapM_ processFile xs
processFile (File (File_Attrs path) content) = writeFile path content
