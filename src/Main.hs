module Main where

import System.Environment
import System.IO
import Text.XML.HaXml
import Text.XML.HaXml.Escape
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
import Data.Char

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
 return (render $ htmlprint $ map myFun $ toContents $ Multifile files)
s = xmlEscapeContent  stdXmlEscaper 
create xs = create' xs >>= \x -> case x of 
   (Left x) -> putStrLn ("unknown file "++x)
   (Right x) -> putStr x

cdatafy x = "<![CDATA[" ++ x ++ "]]>"


g = 
 (\ ch ->
      let
         i = ord ch
      in
         i < 10 || (10<i && i<32) || i >= 127 ||
            case ch of
               '\'' -> True
               '\"' -> True
               '&' -> True
               '<' -> True
               '>' -> True
               _ -> False
      )

myFun (CString a b c) | any g b =  CString a (cdatafy b) c
                      | otherwise =  CString a b c
myFun (CElem e i) = CElem (myFun' e) i
myFun  x = x

myFun' (Elem a b cs) = Elem a b (map myFun cs)

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


