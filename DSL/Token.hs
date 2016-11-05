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
           | Dot
           | Colon
           | Comma
           | Assign
           | Foldr
           | CommentOut
           | Basetype
           | Eol
           deriving (Show,Eq)
