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
          putStrLn "File read successfully"
          putStrLn "Parsing..."

          -- Output tests
--          putStrLn (snd (head (parse item contents)))
--          putStrLn (show (fst (head (parse item contents))))
--          putStrLn (show (parse (item <|> return 'd') "abc"))
          -- putStrLn (show (parse var "abc"))
          -- putStrLn (show (parse exprP "((\\x.x) (y))"))

          -- Parse and print tree to console
          case parse exprP contents of
            []          -> error "Parsing failed"
            [(x,"\n")]  -> do putStrLn "Parsed as:" 
                              putStrLn (show x)
            _           -> error "Parsing failed"


          -- Generate output C file
          putStrLn "Generating output..."
          code <- generator (fst (head (parse exprP contents)))
          writeFile "output.c" code
          
          putStrLn "Success!"

