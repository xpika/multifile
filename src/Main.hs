module Main where

import System.Environment
import System.IO
import Text.XML.HaXml
import Text.XML.HaXml.XmlContent.Haskell
 
import Extsubset
import Data.Either
import Control.Monad
import System.Directory

import Text.PrettyPrint
import Data.Maybe
import Data.Either

import Control.Monad.Trans.Except
import Control.Monad.Trans.Class 

isRegularPath :: FilePath -> Bool 
isRegularPath f = f /= "." && f /= ".."

isRegularDirectory f = fmap (== isRegularPath f) (doesDirectoryExist f) 

doesPathExist' f = liftM2 (||) (isRegularDirectory f) (doesFileExist f)

create' xs = runExceptT $ do
 files <- forM xs $ \filePath -> do
   b <- lift $ doesPathExist' filePath
   if b then do
     content <- lift $ readFile filePath
     return (File (File_Attrs filePath) content )
   else throwE filePath
 return (render $ htmlprint $ toContents $ Multifile files)

create xs = create' xs >>= \x -> case x of 
   (Left x) -> putStrLn "unknown file "
   (Right x) -> putStr x

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
 z _             = putStr "unknown usage"

processFiles (Multifile xs) = mapM_ processFile xs
processFile (File (File_Attrs path) content) = writeFile path content


