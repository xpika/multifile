module Main where


import Text.XMLParser
import System.Environment
import System.IO


main :: IO ()
main = do 
       x <- getContents    
       putStrLn x
