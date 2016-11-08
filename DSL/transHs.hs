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
  Accept (Program ss) -> header ++ unlines (fmap transHs_ ss)

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
    aboutType = name ++ " :: " ++ showTypeHs typ
    aboutExpr  = name ++ " = " ++ showExprHs expr

-- f :: type -> type
-- f x = expr
defineFun name args typ expr = aboutType ++ "\n" ++ aboutExpr
  where
    aboutType = name ++ " :: " ++ showTypeHs typ
    aboutExpr  = name ++ " " ++ unwords args ++ " = " ++ showExprHs expr

defineCata name args typ (FOLDR f_e e_e) = aboutType ++ "\n" ++ aboutExpr
                                       ++ "\n  where\n    " ++ aboutWhere
  where
    f = showExprHs f_e
    e = showExprHs e_e
    aboutType = name ++ " :: " ++ showTypeHs typ
    aboutExpr = "foldF s"
    aboutWhere = "s (Inl One) = " ++ e ++ "\n    " ++ 
                 "s (Inr (Cross a b)) = " ++ f ++ " a b"

header = unlines [
        "{-# LANGUAGE ExistentialQuantification #-}",
        "{-# LANGUAGE ConstrainedClassMethods #-}",
        "{-# LANGUAGE FlexibleContexts #-}",
        "{-# LANGUAGE TypeOperators #-}",
        "import ListCata",
        "import Data.List (union)" ]

-- main = do
--   putStrLn $ transHs_ (BIND "sum" ["xs"] (FUN (LISTty INT) INT) (FOLDR (VAR "f") (NAT 0)))
