{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
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

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\inp -> [])

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P (\inp -> case parse p inp of
                          []        -> parse q inp
                          [(v,out)] -> [(v,out)])


-- Parsing function
parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp



-- Grammar
item :: Parser Char
item = P (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,xs)])

var :: Parser Term
var = P (\inp -> case inp of
                    [] -> []
                    (x:xs) -> [(Var x,xs)])

abs :: Parser Term
abs = P (\inp -> case inp of
                    [] -> []
                    ('(':'\\':c:xs) -> [(Abs c (Var c),xs)])



-- Lambda calculus data type
data Term = Var Char
           | Abs Char Term
           | App Term Term
           | Succ Term

instance Show Term where
  show (Var c) = "Var " ++ [c]
  show (Abs c t) = "Abs " ++ [c] ++ show t
  show (App t u) = "App " ++ show t ++ show u
  show (Succ t) = "Succ " ++ show t

