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



-- Parsing functions
item :: Parser Char
item = P (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,xs)])

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

char :: Char -> Parser Char
char x = sat (== x)

lower :: Parser Char
lower = sat isLower

digit :: Parser Char
digit = sat isDigit

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp


-- Grammar
exprP :: Parser Term
exprP = do _ <- char '('
           t <- termP
           _ <- char ')'
           return t

termP :: Parser Term
termP = do appP <|> absP <|> succP <|> numP <|> varP

varP :: Parser Term
varP = do v <- lower
          return (Var v)

numP :: Parser Term
numP = do n <- nat
          return (Num n)

absP :: Parser Term
absP = do _ <- char '\\'
          c <- item
          _ <- char '.'
          t <- exprP
          return (Abs c t)

appP :: Parser Term
appP = do t <- exprP
          _ <- char ' '
          u <- exprP
          return (App t u)

succP :: Parser Term
succP = do _ <- char 'S'
           _ <- char ' '
           t <- termP
           return (Succ t)
                   

-- AST data type
data Term = Var Char
           | Num Int
           | Abs Char Term
           | App Term Term
           | Succ Term
           deriving Show
