module Token where

data Token = Int Int
           | Bool Bool
           | Var String
           | Eq
           | And
           | Or
           | Leq
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
           | Itype
           | Otype
           | Instance
           | LLabel
           | RLabel
           | Eol
           deriving (Show,Eq)
