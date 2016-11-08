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
  VAR _ -> case varType of
            FUN _ _ -> declareFun varName varType varExpr
            _       -> declareConst varName varType varExpr
  FOLDR _ _ -> declareRecFun varName varType varExpr
  _ -> case varType of
        FUN _ _ -> defineFun varName varArgs varType varExpr
        _       -> declareConst varName varType varExpr

header bt lx = unlines $ [ "" ]
