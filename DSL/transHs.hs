module TransHs ( transHs ) where

import Base
import Lex
import Parse
import Syntax
import Data.Map as Map
import Data.List (isInfixOf)
import Debug.Trace

transHs :: Result Program -> String
transHs prog = case prog of
  Reject err -> ""
  Accept (Program ss) -> ""

transHs_ (LEFT _) = ""
transHs_ (RIGHT _) = ""
transHs_ (BASETYPE _) = ""
transHs_ CommentOut = ""
transHs_ (BIND varName varArgs varType varExpr) = case varExpr of
  _ -> define varName varArgs varType varExpr
--  LIST [_]
--  PAIR _ _
--  GET _ _
--  IF    {cond::_,  tru::_, fal::_} 
--  FOLDR {f::_, e::_}

define name args typ expr = name ++ " = " ++ showExprHs expr

header bt lx = unlines $ [ "" ]

main = do
  print $ transHs_ (BIND "b" [] BOOL (B True))
  print $ transHs_ (BIND "x" [] INT (NAT 111))
  print $ transHs_ (BIND "w" [] INT (APP (VAR "f") (VAR "x")))
  print $ transHs_ (BIND "y" [] INT (PLUS (NAT 1) (NAT 2)))
  print $ transHs_ (BIND "z" [] BOOL (AND (B False) (OR (B True) (AND (B True) (VAR "b")))))
