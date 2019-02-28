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
import qualified Text.PrettyPrint.HughesPJ as Pretty
import Text.XML.HaXml.Namespaces
 
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

main = do 
       args <- getArgs
       case args of 
         [] -> do
           x <- getContents       
           extractMultiFile x
         x -> z x
 where 
 z ("--help":xs) = help
 z ("--h":xs) = help
 z ("--create":xs) = create xs
 z ("-c":xs) = create xs
 z ("--edit":xs) = edit xs
 z ("-e":xs) = edit xs
 z _             = putStr "unknown usage"
 help = do putStrLn "usage:"
           putStrLn "multifile <NOARGS> # extract the multifile"
           putStrLn "multifile --create (or -c) [FILES]"
           putStrLn "multifile --edit (or -e) [FILES] # open multiple files via editor"
           putStrLn "multifile --help (or -h) # show this help message"

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

myFun (CString a b c) | any g b =  CString True (cdatafy b) c
                      | otherwise =  CString a b c
myFun (CElem e i) = CElem (myFun' e) i
myFun  x = x

myFun' (Elem a b cs) = Elem a b (map myFun cs) 


run x = print 2



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



{-

htmlprint2 :: [Content i] -> Pretty.Doc
htmlprint2 = Pretty.cat . map cprint . foldrefs
  where
  foldrefs [] = []
  foldrefs (CString ws s1 i:CRef r _:CString _ s2 _:cs) =
              CString ws (s1++"&"++ref r++";"++s2) i: foldrefs cs
  foldrefs (c:cs) = c : foldrefs cs
  ref (RefEntity n) = n -- Actually, should look-up symtable.
  ref (RefChar s) = show s

  cprint (CElem e _)      = element e
  cprint (CString ws s _) = Pretty.cat (map Pretty.text (fmt 60
                                             ((if ws then id else id) s)))
  cprint (CRef r _)       = Pretty.text ("&"++ref r++";")
  cprint (CMisc _ _)      = Pretty.empty

  element (Elem n as []) = Pretty.text "<"               Pretty.<>
                           Pretty.text (printableName n) Pretty.<>
                           attrs as                      Pretty.<>
                           Pretty.text " />"
  element (Elem n as cs) =
                    --  ( Pretty.text "<"   Pretty.<>
                    --    Pretty.text n     Pretty.<>
                    --    attrs as          Pretty.<>
                    --    Pretty.text ">")  Pretty.$$
                    --  Pretty.nest 6 (htmlprint cs)  Pretty.$$
                    --  ( Pretty.text "</"  Pretty.<>
                    --    Pretty.text n     Pretty.<>
                    --    Pretty.text ">" )
                        Pretty.fcat [ ( Pretty.text "<"               Pretty.<>
                                        Pretty.text (printableName n) Pretty.<>
                                        attrs as                      Pretty.<>
                                        Pretty.text ">")
                                    , Pretty.nest 4 (htmlprint cs)
                                    , ( Pretty.text "</"              Pretty.<>
                                        Pretty.text (printableName n) Pretty.<>
                                        Pretty.text ">" )
                                    ]

  attrs = Pretty.cat . map attribute
  attribute (n,v@(AttValue _)) =
               Pretty.text " "               Pretty.<>
               Pretty.text (printableName n) Pretty.<>
               Pretty.text "='"              Pretty.<>
               Pretty.text (show v)          Pretty.<>
               Pretty.text "'"

  fmt _ [] = []
  fmt n s  = let (top,bot) = splitAt n s
                 (word,left) = keepUntil isSpace (reverse top)
             in if length top < n then [s]
                else if not (null left) then
                     reverse left: fmt n (word++bot)
                else let (big,rest) = keepUntil isSpace s
                     in reverse big: fmt n rest

  deSpace []     = []
  deSpace (c:cs) | c=='\n'   = deSpace (' ':cs)
                 | isSpace c = c : deSpace (dropWhile isSpace cs)
                 | otherwise = c : deSpace cs

  keepUntil p xs = select p ([],xs)
      where select _ (ls,[])     = (ls,[])
            select q (ls,(y:ys)) | q y       = (ls,y:ys)
                                 | otherwise = select q (y:ls,ys)


-}
