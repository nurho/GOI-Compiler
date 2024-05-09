module Fragments where

-- Application
appFrag :: String -> String -> String -> String -> String -> String -> String
appFrag ltop lright lleft ltop_out lright_out lleft_out =
  "  // Application Top\n\
  \  " ++ ltop ++ ": PUSHLEFT; goto " ++ lleft_out ++ ";\n\
  \  // Application Right\n\
  \  " ++ lright ++ ": PUSHRIGHT; goto " ++ lleft_out ++ ";\n\
  \  // Application Left\n\
  \  " ++ lleft ++ ": POP; if (carry == LEFT) goto " ++ ltop_out ++"; else goto " ++ lright_out ++ "; \n\n"

-- Number
numFrag :: String -> String -> Int -> String
numFrag ltop ltop_out num =
  "  // Constant\n\
  \  " ++ ltop ++ ": pushNum(" ++ show num ++ "); printf(\"Push num\\n\"); num_set = 1;\
  \ goto " ++ ltop_out ++ ";\n\n"

-- Successor
succFrag :: String -> String -> String -> String -> String
succFrag ltop lleft ltop_out lleft_out = 
  "  // Successor top\n\
  \  " ++ ltop ++ ": printf(\"Succ visit\\n\"); goto " ++ lleft_out ++ ";\n\
  \  // Successor bottom\n\
  \  " ++ lleft ++ ": printf(\"Succ increment\\n\");\
  \ num++; goto " ++ ltop_out ++ ";\n\n"

-- Abstraction
absFrag :: String -> String -> String -> String -> String -> String -> String
absFrag ltop lleft lright ltop_out lleft_out lright_out = 
  "  // Abstraction Top\n\  
  \  " ++ ltop ++ ": printf(\"Abs top\\n\"); pop(); if (carry == LEFT) goto\
  \ " ++ lright_out ++ "; else goto " ++ lleft_out ++ ";\n\
  \  // Abstraction Left\n\
  \  " ++ lleft ++ ": printf(\"Abs left\\n\"); pushRight(); goto " ++ ltop_out ++ ";\n\
  \  // Abstraction Right\n\
  \  " ++ lright ++ ": printf(\"Abs right\\n\"); pushLeft(); goto " ++ ltop_out ++ ";\n\n"
