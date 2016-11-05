{-# Language NoMonomorphismRestriction #-}
module Main where

import System.Process
import System.IO
import System.Directory

import System.Environment
import System.IO
import Text.XML.HaXml hiding (info,Parser)
import Text.XML.HaXml.Escape
import Text.XML.HaXml.XmlContent.Haskell hiding (Parser)
 
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

import Options.Applicative

isRegularPath :: FilePath -> Bool 
isRegularPath f = f /= "." && f /= ".."

isRegularDirectory f = fmap (== isRegularPath f) (doesDirectoryExist f) 

doesPathExist' f = liftM2 (||) (isRegularDirectory f) (doesFileExist f)

create'' dirStack xs = do
 files <- forM (filter isRegularPath xs) $ \filePath -> do
   let filePath' = dirStack++filePath
   properFile <- lift $ doesFileExist filePath'
   properDir <- lift $ doesDirectoryExist filePath'
   if properDir then do
     filePaths <- lift $ listDirectory filePath'
     files' <- create'' (dirStack ++ ((filter (/='/') filePath) ++ "/")) filePaths
     return files'
   else if properFile then do
     content <- lift $ readFile filePath'
     return [File (File_Attrs filePath') content ]
   else  
     throwE filePath'
 return (concat files)

create' files =runExceptT $ do 
   files' <- create'' [] files
   return $ (render $ htmlprint $ map myFun $ toContents $ Multifile files')


s = xmlEscapeContent  stdXmlEscaper 

create xs = create' xs >>= \x -> case x of 
   (Left x) -> putStrLn ("unknown file "++x++"\n")
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


run x = print 2

main = do 
       args <- getArgs
       case args of 
         [] -> do
           x <- getContents       
           extractMultiFile x
         x -> z x
 where 
 z ("--create":xs) = create xs
 z ("-c":xs) = create xs
 z ("--edit":xs) = edit xs
 z ("-e":xs) = edit xs
 z _             = putStr "unknown usage"

edit xs = do
 dir <- getTemporaryDirectory
 (filename,handle) <- openTempFile dir "a"
 hClose handle
 eitherMultifile <- create' xs
 case eitherMultifile of 
  (Right multifile) -> do
    writeFile filename multifile
    maybeEditor <- lookupEnv "EDITOR"
    let editor = fromMaybe "vim" maybeEditor
    system (editor++" "++filename)
    x <- readFile filename
    extractMultiFile x
  (Left errorMsg) -> putStrLn ("file: "++errorMsg++" does not exist.")
 return ()

extractMultiFile x = do 
        let p = (readXml x :: Either String Multifile)
        either print processFiles p

processFiles (Multifile xs) = mapM_ processFile xs
processFile (File (File_Attrs path) content) = writeFile path content
