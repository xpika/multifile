module Main where

import Text.XMLParser
import System.Environment
import System.IO
import Text.XML.HaXml.XmlContent.Haskell
import Extsubset
import Data.Either

main :: IO ()
main = do 
       x <- getContents    
       let p = (readXml x :: Either String Multifile)
       either print processFiles p

processFiles (Multifile xs) = mapM_ processFile xs
processFile (File (File_Attrs path) content) = writeFile path content
