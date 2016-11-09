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

type MODE = String

transHs :: Result Program -> MODE -> IO String
transHs prog mode = case prog of
  Reject err -> return ""
  Accept (Program ss) ->
    case getInfo ss of
      Reject err -> return ""
      Accept (_,_,_,lx,lq) ->
        return $ header lx lq ++ unlines (fmap transHs_ ss) ++ footer mode

showFuns :: [Expr] -> String
showFuns [f] = "Just . " ++ showExprHs f
showFuns (f:fs) = "Just . " ++ showExprHs f ++ " , " ++ showFuns fs

transHs_ (LEFT ll)  = "lfuns = [ " ++ showFuns ll ++ " ]"
transHs_ (RIGHT rr) = "rfuns = [ " ++ showFuns rr ++ " ]"
transHs_ (INPUT xs) = "input_data = " ++ showExprHs (LIST xs)
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
                                           ++ aboutWhere
  where
    f = showExprHs f_e
    e = showExprHs e_e
    -- aboutType = name ++ " :: " ++ showTypeHs typ
    aboutType = ""
    aboutExpr = name ++ " " ++ unwords args ++ " = foldF s"
    aboutWhere = "\n  where\n" ++
                 "    s (Inl One) = " ++ e ++ "\n" ++ 
                 "    s (Inr (Cross a b)) = " ++ f ++ " a b"

header lx lq =
        unlines $ [
        "import ListCata",
        "import Data.List (union)" ]
        ++ if lx then ["leq_lexico = (<=)"] else []
        ++ if lq then ["leq = (<=)"] else []

footer mode = aboutSolver ++ "\n" ++ aboutMain
  where
    aboutSolver = "thin_or_greedy = solverMain (lfuns,rfuns) p r q"
    aboutMain = "main =" ++
                "  print.fromList.thin_or_greedy " ++ mode ++ " $ toList input_data"

-- main = do
--   putStrLn $ transHs_ (BIND "sum" ["xs"] (FUN (LISTty INT) INT) (FOLDR (VAR "f") (NAT 0)))
