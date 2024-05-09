module CodeGenerator where

-- Internal modules
import Parsing
import Fragments

-- Retrieve the boilerplate code from file
getBoilerplate :: IO String
getBoilerplate = readFile "boilerplate.txt"

-- Interface for code generation
generator :: Term -> IO String
generator t = do b <- getBoilerplate
                 return (b ++ output t 1 (findLabels t 1 []) ++ finalCode)

-- Main code generation function
output :: Term -> Int -> [(Char,Int)] -> String
output (Var _) _ _    = []
output (Num n) l _    = numFrag (lab l) (lab (l+1)) n
output (Abs c t) l cs = absFrag (lab l) (lab (l+1)) (lab (l+2)) (lab (l-1)) (lab (getLink c cs)) (lab (l+3)) ++ output t (l+3) cs
output (App t u) l cs = appFrag (lab l) (lab (l+1)) (lab (l+2)) (lab (l-1)) (lab (l+3)) (lab (l+(countLabels u))) ++ output t (l+3) cs ++ output u (l+6) cs
output (Succ t)  l cs = succFrag (lab l) (lab (l+1)) (lab (l-1)) (lab (l+2)) ++ output t (l+3) cs

-- Traverse a tree to find links
findLabels :: Term -> Int -> [(Char,Int)] -> [(Char,Int)]
findLabels (Var c) l cs   = if (c,l) `elem` cs then error ("Non-linear var: " ++ [c]) else cs ++ [(c,l)]
findLabels (Num _) _  _   = []
findLabels (Abs _ t) l cs = findLabels t (l+3) cs
findLabels (App t u) l cs = findLabels t ((l+3) + countLabels u) cs ++ findLabels u (l+3) cs
findLabels (Succ t)  l cs = findLabels t (l+1) cs

-- Count labels required for a subtree
countLabels :: Term -> Int
countLabels (Var _)    = 0
countLabels (Num _)    = 1
countLabels (Abs _ t)  = countLabels t + 3
countLabels (App t u)  = countLabels t + countLabels u + 3
countLabels (Succ t)   = countLabels t + 2

-- Utility functions:
-- Make a label string from an Int
lab :: Int -> String
lab n = "L" ++ show n

-- Look up bound variable connections from data structure
getLink :: Char -> [(Char,Int)] -> Int
getLink c cs = case lookup c cs of
                 Just x -> x
                 Nothing -> error "Logic error: missing Var"
