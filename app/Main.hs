module Main where

-- External imports
import System.Environment
import System.Directory
import Control.Monad
import Control.Applicative
import Data.Typeable

-- Internal modules
import Parsing
import CodeGenerator

main :: IO ()
main = do args <- getArgs
          -- Get input file path from first arg
          when (null args) $ error "No input file declared"
          let filepath = head args

          -- Attempt to find input file
          putStrLn ("\nFinding file \'" ++ filepath ++ "\'")
          filefound <- doesFileExist filepath
          unless filefound $ error "File not found"

          -- Read contents of input file
          contents <- readFile filepath

          -- Output tests
--          putStrLn (snd (head (parse item contents)))
--          putStrLn (show (fst (head (parse item contents))))
--          putStrLn (show (parse (item <|> return 'd') "abc"))
          putStrLn (show (parse var "abc"))

          -- Generate output .c file
          generator (fst (head (parse var "abc")))

