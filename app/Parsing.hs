module Parsing where

import Control.Applicative
import Data.Char

parse :: String -> String
parse s = s

newtype Parser a = P (String -> [(a,String)])
