module CodeGenerator where

-- External imports
import System.Environment
import System.Directory
import Control.Monad

-- Internal modules
import Parsing
 
generator :: Term -> IO ()
generator t = writeFile "output.c" "test"
