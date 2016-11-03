module Syntax where

data TY  = INT 
         | BOOL 
         | LISTty TY
         | PAIRty TY TY
         | FUN TY TY 
         | BOTTOM String 
           deriving (Show,Read,Eq)

data Program = Program [Expr] deriving (Show,Read)

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
          | GET Expr Expr
          | IF {cond::Expr, tru::Expr, fal::Expr} 
          | ABS {var::String, ty::TY, e::Expr} 
          | BIND {var::String, ty::TY, e::Expr, in_::Expr}
          | REC   {f::String, ty::TY, var::String, e::Expr, in_::Expr}
          | FOLDR {h::Expr, e::Expr}
          | EOF
          deriving (Show,Read)

