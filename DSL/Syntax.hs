module Syntax where

data TY  = INT
         | BOOL
         | LISTty TY
         | PAIRty TY TY
         | FUN TY TY 
         | BOTTOM String 
           deriving (Read,Eq)

instance Show TY where
  show INT = "Int"
  show BOOL = "Bool"
  show (LISTty t) = "List "++show t
  show (PAIRty t1 t2) = "Pair "++show t1++" "++show t2
  show (FUN t1 t2) = show t1++" -> "++show t2
  show (BOTTOM s) = "BOTTOM "++s



data Program = Program [Sentence] deriving (Show,Read)

data Sentence = BIND {name::String, ty2::TY, e2::Expr} 
              | CommentOut
                deriving (Show,Read)

data Expr = NAT Int 
          | B Bool
          | VAR String 
          | PLUS Expr Expr
          | MINUS Expr Expr
          | TIMES Expr Expr
          | EQU  Expr Expr
          | AND  Expr Expr
          | OR Expr Expr
          | APP Expr Expr
          | LIST [Expr]
          | PAIR Expr Expr
          | GET Expr Expr
          | IF    {cond::Expr, tru::Expr, fal::Expr} 
          | ABS   {var::String, ty::TY, e::Expr} 
          | FOLDR {f::Expr, e::Expr}
          | EOF
            deriving (Read)

instance Show Expr where
  show (NAT a) = show a
  show (B a) = show a
  show (VAR a) = a
  show (PAIR a b) = "(" ++ show a ++ "," ++ show b ++ ")"

-- main = do
--   print $ NAT 100
--   print $ VAR "f"
--   print $ PAIR (NAT 100) (NAT 300)
