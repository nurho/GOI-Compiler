module Main where

-- External imports
import System.Environment
import System.Directory
import Control.Monad
import Data.Typeable

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
          putStrLn (snd (head (parse item contents)))
          putStrLn (show (fst (head (parse item contents))))
