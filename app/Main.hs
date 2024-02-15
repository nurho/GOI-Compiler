module Main where

-- External imports
import System.Environment
import System.Directory
import Control.Monad

-- Internal modules
import Parsing

main :: IO ()
main = do args <- getArgs
          when (null args) $ error "No input file declared"
          let filename = head args

          putStrLn ("\nFinding file \'" ++ filename ++ "\'")
          filefound <- doesFileExist filename
          unless filefound $ error "File not found"

          contents <- readFile filename
          putStrLn (parse contents)
