module Evaluation (evalFile,eval) where

import Base
import Lex
import Parse
import Syntax
import Data.Map as Map
import Debug.Trace

evalFile s = eval.parse.scanTokens <$> readFile s

eval prog = case prog of
  Reject err -> show err
  Accept (Program ss) -> header ++ (unlines $ fmap eval_ ss) ++ "\n(check-sat)"

eval_ CommentOut = ""
eval_ (BIND varName vartypes varType varExpr) = case varExpr of
  NAT _ -> declareConst varName varType varExpr
  B _   -> declareConst varName varType varExpr
  PAIR _ _ -> case varType of
                FUN _ _ -> defineFun varName vartypes varType varExpr
                _       -> declareConst varName varType varExpr
  VAR _ -> case varType of
            FUN _ _ -> declareFun varName varType varExpr
            _       -> declareConst varName varType varExpr
  _ -> ""


-- [x,y] -> (a -> b -> c) -> "((x a) (y b)) (c)"
argTuple :: [String] -> TY -> String
argTuple as ts = concat $ zipWith aux as (types $ ts)
  where aux x t = "(" ++ x ++ t ++ ")"
-- FUN a (FUN b c) -> [a,b]
types :: TY -> [String]
types (FUN a b) = showType a : types b
types _         = []

cod :: TY -> String
cod (FUN a b) = cod b
cod a = showType a

-- (declare-const x typ)
-- (assert (= x v))
declareConst x typ v = a1 ++ "\n" ++ a2
  where
    a1 = "(declare-const " ++ x ++ " " ++ showType typ ++ ")"
    a2 = "(assert (= " ++ x ++ " " ++ showExpr v ++ "))"


-- (declare-fun f ((t1) (t2) .. ) (tn))
-- (assert (forall ((x1 t1) (x2 t2) ..)
--         (= (f x1 x2 ..) (hoge x1 x2))))
declareFun f typ v = a1 ++ "\n" ++ a2 ++ "\n" ++ a3
  where
    a1 = "(declare-fun " ++ f ++ " " ++ showType typ ++ ")"
    a2 = "(assert (forall (" ++ (argTuple argSequence typ) ++ ")"
    a3 = "(= " ++ mkApp f ++ mkApp (showExpr v) ++ ")))"
    sz = length (types typ)
    argSequence = fmap (\i -> "x" ++ show i ++ " ") [1..sz]
    mkApp f = "(" ++ f ++ " " ++ (concat argSequence) ++ ")"


-- (define-fun f ((x t1) (y t2)) t3 
--   hogehoge )
defineFun f as typ expr = a1 ++ "\n" ++ a2
  where
    a1 = "(define-fun " ++ f ++ " (" ++ argTuple as typ ++ ")" ++ cod typ
    a2 = showExpr expr ++ ")"


header = "\
\(declare-datatypes (T1 T2) ((Pair (mk-pair (fst T1) (snd T2)))))\n\
\(define-fun cons ((x Int) (xs (List Int))) (List Int)\n\
\  (insert x xs))\n\
\(define-fun outr ((x Int) (xs (List Int))) (List Int)\n\
\  xs)"


