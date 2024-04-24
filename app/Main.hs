module Main where

-- External imports
import System.Environment
import System.Directory
import Control.Monad
import Control.Applicative
import Data.Typeable

-- Internal modules
import Parsing

main :: IO ()
main = do args <- getArgs
          -- Get input path from first arg
          when (null args) $ error "No input file declared"
          let filepath = head args

          -- Attempt to find file from 
          putStrLn ("\nFinding file \'" ++ filepath ++ "\'")
          filefound <- doesFileExist filepath
          unless filefound $ error "File not found"

          contents <- readFile filepath
--          putStrLn (snd (head (parse item contents)))
--          putStrLn (show (fst (head (parse item contents))))
--          putStrLn (show (parse (item <|> return 'd') "abc"))
          putStrLn (show (parse var "abc"))
