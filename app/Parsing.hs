module Parsing where

import Control.Applicative
import Data.Char

-- Parser type
newtype Parser a = P (String -> [(a,String)])

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p = P (\inp -> case parse p inp of
                          []        -> []
                          [(v,out)] -> [(g v, out)])

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P (\inp -> [(v,inp)])

  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = P (\inp -> case parse pg inp of
                   []        -> []
                   [(g,out)] -> parse (fmap g px) out)

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\inp -> case parse p inp of
                         []        -> []
                         [(v,out)] -> parse (f v) out)

-- Parsing function
parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

-- Grammar
item :: Parser Char
item = P (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,xs)])

-- AST
data Var = Char 

data AST = Var
           | Abs Var AST
           | App AST AST
           | Succ AST
