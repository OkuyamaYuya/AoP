module TransHs (transHs) where

import Base
import Lex
import Parse
import Syntax
import TransZ3 (getInfo)
import Data.List (isInfixOf)
import Data.Map as Map
import System.Directory
import Debug.Trace

transHs :: Result Program -> IO String
transHs prog = case prog of
  Reject err -> return ""
  Accept (Program ss) ->
    case getInfo ss of
      Reject err -> return ""
      Accept (ll,rr,_,lx) ->
        return $ header lx ++ unlines (fmap transHs_ ss) ++ footer ll rr

transHs_ (LEFT _) = ""
transHs_ (RIGHT _) = ""
transHs_ (BASETYPE _) = ""
transHs_ CommentOut = ""
transHs_ (BIND varName varArgs varType varExpr) = case varExpr of
  FOLDR _ _ -> defineCata varName varArgs varType varExpr
  _
    | Prelude.null varArgs -> defineConst varName varArgs varType varExpr
    | otherwise -> defineFun varName varArgs varType varExpr

-- x :: type
-- x = expr
defineConst name args typ expr = aboutType ++ "\n" ++ aboutExpr
  where
    -- aboutType = name ++ " :: " ++ showTypeHs typ
    aboutType = ""
    aboutExpr  = name ++ " = " ++ showExprHs expr

-- f :: type -> type
-- f x = expr
defineFun name args typ expr = aboutType ++ "\n" ++ aboutExpr
  where
    -- aboutType = name ++ " :: " ++ showTypeHs typ
    aboutType = ""
    aboutExpr  = name ++ " " ++ unwords args ++ " = " ++ showExprHs expr

defineCata name args typ (FOLDR f_e e_e) = aboutType ++ "\n" ++ aboutExpr
                                       ++ "\n  where\n    " ++ aboutWhere
  where
    f = showExprHs f_e
    e = showExprHs e_e
    -- aboutType = name ++ " :: " ++ showTypeHs typ
    aboutType = ""
    aboutExpr = "foldF s"
    aboutWhere = "s (Inl One) = " ++ e ++ "\n    " ++ 
                 "s (Inr (Cross a b)) = " ++ f ++ " a b"

header lx = 
        unlines $ [
        "{-# LANGUAGE ExistentialQuantification #-}",
        "{-# LANGUAGE ConstrainedClassMethods #-}",
        "{-# LANGUAGE FlexibleContexts #-}",
        "{-# LANGUAGE TypeOperators #-}",
        "import ListCata",
        "import Data.List (union)" ]
        ++ if lx then ["leq_lexico = (<=)"] else []


footer lfuns rfuns = sF ++ "\n  where\n" 
                     ++ aboutL ++ "\n" ++ aboutR
                     ++ "\n" ++ aboutSolver
                     ++ "\n" ++ aboutMain
  where
    sF = "sF = (funs1,funs2)"
    aboutL = "    funs1 = [ " ++ showFuns lfuns ++ " ]"
    aboutR = "    funs2 = [ " ++ showFuns rfuns ++ " ]"
    showFuns [f] = "Just . " ++ f
    showFuns (f:fs) = "Just . " ++ f ++ " , " ++ showFuns fs
    aboutSolver = "thin_or_greedy = solverMain sF p r q"
    aboutMain = "main = do\n" ++
                "  print.fromList.thin_or_greedy Greedy $ toList [1,34,2,1,44]"

-- main = do
--   putStrLn $ transHs_ (BIND "sum" ["xs"] (FUN (LISTty INT) INT) (FOLDR (VAR "f") (NAT 0)))
