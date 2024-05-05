module CodeGenerator where

-- External imports

-- Internal modules
import Parsing
import Fragments
 

testTerm :: Term
testTerm = App (Var 'x') (Var 'y')

testTerm2 :: Term
testTerm2 = App (Abs 'x' (Succ (Var 'x'))) (Num 4)

generator :: Term -> IO ()
generator t = writeFile "output.c" (output testTerm2)

-- TODO: Add list of ints as stack
-- Needs two run throughs, one to find label names (connections) then
-- another to generate the code
-- The first run could be a list
-- Could run through counting the labels then build a dictionary of variables
-- and their labels
output :: Term -> Int -> String
output (Var _) _   = []
output (Num n) l   = numFrag (show l+1) (show l) n
output (Abs c t) l = absFrag "xx" "xx" "xx" "xx" "xx" "xx" ++ output t (l+3) 
output (App t u) l = appFrag "xx" "xx" "xx" "xx" "xx" "xx" ++ output t ++ output u
output (Succ t)  l = succFrag "xx" "xx" "xx" "xx" ++ output t

getLabels :: Term -> Int -> [(Char, Int)] -> [(Char, Int)]
getLabels  (Var c) lc ls   = ls ++ (c, lc)
getLabels  (Num n) lc ls   = ls
getLabels  (Abs c t) lc ls = getLabels t (lc + 5) ls
getLabels  (App t u) lc ls = (getLabels t (lc + 5) ls) ++ (getLabels t (lc + 10) ls)
getLabels  (Succ t) lc ls  = 

data LTerm = LVar Char [Int]
           | LNum Int [Int]
           | LAbs Char LTerm [Int]
           | LApp LTerm LTerm [Int]
           | LSucc LTerm [Int]
