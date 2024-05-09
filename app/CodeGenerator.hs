module CodeGenerator where

-- External imports
import Data.List

-- Internal modules
import Parsing
import Fragments


testTerm :: Term
testTerm = App (Var 'x') (Var 'y')

testTerm2 :: Term
testTerm2 = App (Abs 'x' (Succ (Var 'x'))) (Num 4)

generator :: Term -> IO String
generator t = do b <- getBoilerplate
                 return (b ++ output t 1 (findLabels t 1) ++ "\n}")
-- TODO: Add end boilerplate i.e. L0 label                 

getBoilerplate :: IO String
getBoilerplate = readFile "boilerplate.txt"

-- TODO: Add list of ints as stack
-- Needs two run throughs, one to find label names (connections) then
-- another to generate the code
-- The first run could be a list
-- Could run through counting the labels then build a dictionary of variables
-- and their labels

-- Make a label string from an Int
lab :: Int -> String
lab n = "L" ++ show n

getLink :: Char -> [(Char,Int)] -> Int
getLink c cs = case lookup c cs of
                 Just x -> x
                 Nothing -> error "Logic error: missing Var"


-- TODO: Fix label numbers
output :: Term -> Int -> [(Char,Int)] -> String
output (Var _) _ _    = []
output (Num n) l _    = numFrag (lab l) (lab (l+1)) n
output (Abs c t) l cs = absFrag (lab l) (lab (l+1)) (lab (l+2)) (lab (l-1)) (lab (getLink c cs)) (lab (l+3)) ++ output t (l+3) cs
output (App t u) l cs = appFrag (lab l) (lab (l+1)) (lab (l+2)) (lab (l-1)) (lab (l+3)) (lab (l+(countLabels u))) ++ output t (l+3) cs ++ output u (l+6) cs
output (Succ t)  l cs = succFrag (lab l) (lab (l+1)) (lab (l-1)) (lab (l+2)) ++ output t (l+3) cs

findLabels :: Term -> Int -> [(Char,Int)]
findLabels (Var c) l   = [(c,l)]
findLabels (Num _) _   = []
findLabels (Abs _ t) l = findLabels t (l+3)
findLabels (App t u) l = findLabels t ((l+3) + countLabels u) ++ findLabels u (l+3)
findLabels (Succ t)  l = findLabels t (l+1)

countLabels :: Term -> Int
countLabels (Var _)    = 0
countLabels (Num _)    = 1
countLabels (Abs _ t)  = countLabels t + 3
countLabels (App t u)  = countLabels t + countLabels u + 3
countLabels (Succ t)   = countLabels t + 2


-- getLabels :: Term -> Int -> [(Char, Int)] -> [(Char, Int)]
-- getLabels  (Var c) lc ls   = ls ++ (c, lc)
-- getLabels  (Num n) lc ls   = ls
-- getLabels  (Abs c t) lc ls = getLabels t (lc + 5) ls
-- getLabels  (App t u) lc ls = (getLabels t (lc + 5) ls) ++ (getLabels t (lc + 10) ls)
-- getLabels  (Succ t) lc ls  = 
--
-- data LTerm = LVar Char [Int]
--            | LNum Int [Int]
--            | LAbs Char LTerm [Int]
--            | LApp LTerm LTerm [Int]
--            | LSucc LTerm [Int]
