module Main where

import System.Environment
import System.IO
import Text.XML.HaXml
import Text.XML.HaXml.XmlContent.Haskell
 
import Extsubset
import Data.Either
import Control.Monad
import System.Directory

import Options.Applicative

import Text.PrettyPrint

create' xs = do
 files <- forM xs $ \filePath -> do
   content <- readFile filePath
   return ( File (File_Attrs filePath) content )
 return (render $ htmlprint $ toContents $ Multifile files)

create xs = create' xs >>= putStr

 -- data Args = Args {

dir x = getDirectoryContents x >>= create 

main :: IO ()
main = do 
       args <- getArgs
       case args of 
         [] -> do
           x <- getContents    
           let p = (readXml x :: Either String Multifile)
           either print processFiles p
         x -> z x
 where 
 z ("-create":xs) = create xs
 z ("-c":xs) = create xs
 z ("-dir":x:[]) = dir x
 z _             = putStr "unknown usage"

processFiles (Multifile xs) = mapM_ processFile xs
processFile (File (File_Attrs path) content) = writeFile path content


