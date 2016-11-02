{

module Lex where

import qualified Token as T

}
%wrapper "basic"

$digit = [0-9]
$lower = [a-z]
$alpha = [a-zA-Z]

tokens :-
    $white+ ;
    "(" { \s -> T.Lparen }
    ")" { \s -> T.Rparen }
    "[" { \s -> T.Lparen2 }
    "]" { \s -> T.Rparen2 }
    "True" { \s -> T.Bool True }
    "False" { \s -> T.Bool False }
    $digit+ { \s -> T.Int (read s) }
    "-" { \s -> T.Minus }
    "+" { \s -> T.Plus }
    "*" { \s -> T.Times }
    "==" { \s -> T.Eq }
    "||" { \s -> T.Or }
    "&&" { \s -> T.And }
    "if" { \s -> T.If }
    "then" { \s -> T.Then }
    "else" { \s -> T.Else }
    "let" { \s -> T.Let }
    "rec" { \s -> T.Rec }
    "in" { \s -> T.In }
    "\" { \s -> T.Lambda }
    "Int" { \s -> T.TyInt }
    "Bool" { \s -> T.TyBool }
    "List" { \s -> T.TyList }
    "->" { \s -> T.Arrow }
    "." { \s -> T.Dot }
    "," { \s -> T.Comma }
    ":" { \s -> T.Colon }
    "=" { \s -> T.Assign }
    $lower [$alpha $digit \_ \’]* { \s -> T.Var s }
{

scanTokens = alexScanTokens

}
