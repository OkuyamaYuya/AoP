module Syntax where

data TY  = INT
         | BOOL
         | LISTty TY
         | PAIRty TY TY
         | FUN TY TY 
         | BOTTOM String 
           deriving (Show,Read,Eq)

class ShowType t where
  showType :: t -> String

instance ShowType TY where
  showType (BOTTOM s) = "BOTTOM "++s
  showType INT = "(Int)"
  showType BOOL = "(Bool)"
  showType (LISTty t) = "(List "++showType t++")"
  showType (PAIRty t1 t2) = "(Pair "++showType t1++" "++showType t2++")"
  showType (FUN t1 t2) = "(" ++ (toTypeStyle.args $ (FUN t1 t2))
    where
      -- can't use higher order function
      -- a in a -> b is always int,bool,list,pair.
      args (FUN a b) = a : args b
      args others    = [others]
      toTypeStyle (a:[]) = ")" ++ showType a
      toTypeStyle (a:as) = showType a ++ toTypeStyle as


data Program = Program [Sentence] deriving (Show,Read)

data Sentence = BIND {name::String, args::[String] , ty2::TY, e2::Expr} 
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
          | IF    {cond::Expr,  tru::Expr, fal::Expr} 
          | FOLDR {f::Expr, e::Expr}
          | EOF
            deriving (Show,Read)

class ShowExpr f where
  showExpr :: f -> String

instance ShowExpr Expr where
  showExpr (NAT a) = show a
  showExpr (B True) = "true"
  showExpr (B False) = "false"
  showExpr (VAR a) = a
  showExpr (PAIR a b) = "(mk-pair " ++ showExpr a ++ " " ++ showExpr b ++ ")"
  showExpr (PLUS a b) = "(+ " ++ showExpr a  ++ showExpr b ++ ")"
  showExpr (APP (APP a b) c) = "(" ++ showExpr a ++ " " ++ showExpr b ++ " " ++ showExpr c ++ ")"
  showExpr (APP a b) = "(" ++ showExpr a ++ " " ++ showExpr b ++ ")"

-- main = do
--   print $ showExpr $ (APP (APP (VAR "leq") (VAR "p2")) (VAR "p1"))
