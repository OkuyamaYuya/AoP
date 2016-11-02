module Syntax where

data TY  = INT 
         | BOOL 
         | LISTtype TY
         | FUN TY TY 
         | BOTTOM String 
           deriving (Show,Read,Eq)

data EXP = NAT Int 
         | B Bool
         | VAR String 
         | PLUS EXP EXP
         | MINUS EXP EXP
         | TIMES EXP EXP
         | EQU  EXP EXP
         | AND  EXP EXP
         | OR EXP EXP
         | APP EXP EXP
         | LIST [EXP]
         | GET EXP EXP
         | IF {cond::EXP, tru::EXP, fal::EXP} 
         | ABS {var::String, ty::TY, e::EXP} 
         | BIND {var::String, ty::TY, e::EXP, in_::EXP}
         | REC  {f::String, ty::TY, var::String, e::EXP, in_::EXP}
         | EOF
         deriving (Show,Read)

