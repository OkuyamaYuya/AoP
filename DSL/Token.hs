module Token where

data Token = Let
           | Rec
           | In
           | Int Int
           | Bool Bool
           | Var String
           | Eq
           | And
           | Or
           | Plus
           | Minus
           | Times
           | Div
           | Lparen
           | Rparen
           | Lparen2
           | Rparen2
           | If
           | Then
           | Else
           | TyInt
           | TyBool
           | TyList
           | Arrow
           | Lambda
           | Dot
           | Colon
           | Comma
           | Assign
           | Foldr
           | Eol
           deriving (Show,Eq)
