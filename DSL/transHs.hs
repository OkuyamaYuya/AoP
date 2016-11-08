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
  Accept (Program ss) -> unlines $ fmap transHs_ ss

transHs_ (LEFT _) = ""
transHs_ (RIGHT _) = ""
transHs_ (BASETYPE _) = ""
transHs_ CommentOut = ""
transHs_ (BIND varName varArgs varType varExpr) = case varExpr of
  FOLDR _ _ -> ""
  _
    | Prelude.null varArgs -> defineConst varName varArgs varType varExpr
    | otherwise -> defineFun varName varArgs varType varExpr

-- x :: type
-- x = expr
defineConst name args typ expr = aboutType ++ "\n" ++ aboutExpr
  where
    aboutType = name ++ " :: " ++ showTypeHs typ
    aboutExpr  = name ++ " = " ++ showExprHs expr

-- f :: type -> type
-- f x = expr
defineFun name args typ expr = aboutType ++ "\n" ++ aboutExpr
  where
    aboutType = name ++ " :: " ++ showTypeHs typ
    aboutExpr  = name ++ " " ++ unwords args ++ " = " ++ showExprHs expr

header bt lx = unlines $ [ "" ]

main = do
  putStrLn $ transHs_ (BIND "f" ["x","y"] (FUN INT INT) (PLUS (VAR "y") (VAR "x")))
  putStrLn $ transHs_ (BIND "z" [] BOOL (AND (B False) (OR (B True) (AND (B True) (VAR "b")))))
