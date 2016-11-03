module Token where

data Token = Int Int
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
           | TyPair
           | Arrow
           | Lambda
           | Dot
           | Colon
           | Comma
           | Assign
           | Foldr
           | CommentOut
           | Eol
           deriving (Show,Eq)
