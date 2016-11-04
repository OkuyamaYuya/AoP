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
  Accept (Program ss) -> header ++ (unlines $ fmap eval_ ss)

eval_ CommentOut = ""
eval_ (BIND varName varType varExpr) = case varExpr of
  NAT _ -> (declareConst varName varType varExpr)
  B _   -> (declareConst varName varType varExpr)
  PAIR _ _ -> (declareConst varName varType varExpr)
  VAR _ -> (declareFun varName varType varExpr)
  _ -> ""

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
    a2 = "(assert (forall (" ++ (argsTuple typ) ++ ")"
    a3 = "(= " ++ mkApp f ++ mkApp (showExpr v) ++ ")))"
    argsTuple as = concat $ zipWith aux argSequence (args $ as)
      where aux x t = "(" ++ x ++ t ++ ")"
    sz = length (args typ)
    mkApp f = "(" ++ f ++ " " ++ (concat argSequence) ++ ")"
    argSequence = fmap (\i -> "x" ++ show i ++ " ") [1..sz]
    args (FUN a b) = showType a : args b
    args _         = []

header = "\
\(declare-datatypes (T1 T2) ((Pair (mk-pair (first T1) (second T2)))))\n\
\(define-fun cons ((x Int) (xs (List Int))) (List Int)\n\
\  (insert x xs))\n\
\(define-fun outr ((x Int) (xs (List Int))) (List Int)\n\
\  xs)"

ss = [ "",
       "x : Int = 1",
       "b : Bool = True",
       "p1 :(Pair Int Int) = (1,2)",
       "f1 : Int -> (List Int) -> List Int = cons",
       "pairPlus : (Pair Int Int)->(Pair Int Int)->(Pair Int Int) ="++
       "\\p1:(Pair Int Int). \\p2:(Pair Int Int). (fst p1,snd p2)",
       -- "sum : (List Int) -> Int = foldr plus 0",
       "" ]

main :: IO()
main = do
  putStrLn $ eval.parse.scanTokens $ unlines ss
  print 0
