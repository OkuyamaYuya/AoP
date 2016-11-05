module Evaluation (evalFile,eval) where

import Base
import Lex
import Parse
import Syntax
import Data.Map as Map
import Debug.Trace

evalFile s = eval.parse.scanTokens <$> readFile s


basetype = PAIRty INT INT
funs = [ "f1" , "f2" ]

eval prog = case prog of
  Reject err -> show err
  Accept (Program ss) ->
    case getFunsBtype ss of
      Reject err -> show err
      Accept (fs,bt) ->
        header bt ++ (unlines $ fmap eval_ ss)
        ++ makeQuery fs bt ++"\n(check-sat)"

getFunsBtype :: [Sentence] -> Result ([String],TY)
getFunsBtype ss =
  let fs = findRight ss
      bt = findBase ss
  in
    case (fs,bt) of
      (Nothing,_) -> Reject "write 'RIGHT'."
      (_,Nothing) -> Reject "write 'BASETYPE'."
      (Just jfs,Just jbt) -> Accept (fmap showExpr jfs,jbt)
  where
    -- find leftmost one.
    findRight [] = Nothing
    findRight ((RIGHT x):xs) = Just x
    findRight (_:xs) = findRight xs
    findBase [] = Nothing
    findBase ((BASETYPE x):xs) = Just x
    findBase (_:xs) = findBase xs

eval_ (LEFT _) = ""
eval_ (RIGHT _) = ""
eval_ (BASETYPE _) = ""
eval_ CommentOut = ""
eval_ (BIND varName varArgs varType varExpr) = case varExpr of
  VAR _ -> case varType of
            FUN _ _ -> declareFun varName varType varExpr
            _       -> declareConst varName varType varExpr
  FOLDR _ _ -> declareRecFun varName varType varExpr
  _ -> case varType of
        FUN _ _ -> defineFun varName varArgs varType varExpr
        _       -> declareConst varName varType varExpr

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
declareConst x typ v = unlines [a1,a2]
  where
    a1 = "(declare-const " ++ x ++ " " ++ showType typ ++ ")"
    a2 = "(assert (= " ++ x ++ " " ++ showExpr v ++ "))"


-- (declare-fun f ((t1) (t2) .. ) (tn))
-- (assert (forall ((x1 t1) (x2 t2) ..)
--         (= (f x1 x2 ..) (hoge x1 x2))))
declareFun f typ v = unlines [a1,a2,a3]
  where
    a1 = "(declare-fun " ++ f ++ " " ++ showType typ ++ ")"
    a2 = "(assert (forall (" ++ (argTuple argSequence typ) ++ ")"
    a3 = "(= " ++ mkApp f ++ mkApp (showExpr v) ++ ")))"
    sz = length (types typ)
    argSequence = fmap (\i -> "x" ++ show i ++ " ") [1..sz]
    mkApp f = "(" ++ f ++ " " ++ (concat argSequence) ++ ")"


-- (define-fun f ((x t1) (y t2)) t3 
--   hogehoge )
defineFun f as typ expr = unlines [a1,a2]
  where
    a1 = "(define-fun " ++ f ++ " (" ++ argTuple as typ ++ ")" ++ cod typ
    a2 = showExpr expr ++ ")"

-- (declare-fun pair-sum ((List (Pair Int Int))) (Pair Int Int))
-- (assert (forall ((xs (List (Pair Int Int))))
--           (ite (= nil xs)
--                (= (mk-pair 0 0)                              (pair-sum xs))
--                (= (pair-plus (head xs) (pair-sum (tail xs))) (pair-sum xs)))))
declareRecFun recfun typ (FOLDR (VAR f) e) = unlines [a1,a2,a3,a4,a5]
  where
    a1 = "(declare-fun " ++ recfun ++ " " ++ showType typ ++ ")"
    a2 = "(assert (forall ((xs " ++ showType (dom typ) ++ "))"
    a3 = "  (ite (= nil xs)"
    a4 = "       (= " ++ showExpr e ++ "    (" ++ recfun ++ " xs))"
    a5 = "       (= (" ++ f ++ " (head xs) (" ++ recfun ++ "(tail xs))) (" ++ recfun ++ " xs)))))"
    dom (FUN a b) = a
declareRecFun recfun typ (FOLDR _ e) = "ERROR"


-- Types of cons,outr,nil depend on a problem user defines.
header bt = unlines [ "",
    "(declare-datatypes (T1 T2) ((Pair (mk-pair (fst T1) (snd T2)))))",
    "(define-fun cons ((x "++showType bt++") (xs (List "++showType bt++"))) (List "++showType bt++")",
    "  (insert x xs))",
    "(define-fun outr ((x "++showType bt++") (xs (List "++showType bt++"))) (List "++showType bt++")",
    "  xs)",
    "(declare-fun leq (Int Int) Bool)",
    "(assert (forall ((x Int) (y Int)) (= (<= x y) (leq x y))))",
    -- "(declare-fun leq_lexico ((List Int) (List Int)) Bool)",
    ""]

makeQuery :: [String] -> TY -> String
makeQuery fs bt = a1 ++ a2 ++ a3
  where
    a1 = unlines $ fmap declareBool fs
    a2 = do
      f <- fs
      mainQuery f fs bt
    a3 = lastQuery fs

-- (declare-const b Bool)
declareBool f = "(declare-const " ++ bf ++ " Bool)"
  where bf = "b" ++ f

-- (assert (not (b1 b2 ..)))
lastQuery fs = "(assert (not (and " ++ aux fs ++ ")))"
  where
    aux xs = concat.prepare $ xs
    prepare xs = fmap (\x->"b"++x++" ") xs

-- Better-Local monotonicity check
-- q : local criterion
-- (assert (forall ((x T)(y T)..)
--  hogehoge
-- ))
mainQuery f fs btype =
  let bf = "b" ++ f in
    unlines $ [ "",
    "(assert (= " ++ bf,
    "    (forall ((xs (List " ++ showType btype ++ ")) (ys (List " ++ 
                 showType btype ++ ")) (a " ++ showType btype ++ "))",
    "    (=> (q ys xs) ",
    "    (=> (p (" ++ f ++ " a ys))",
    "        (or "] ++ fmap (target f) fs ++ ["))))))"]
      where
        target f g = "\t(and (p (" ++ g ++ " a xs)) (q (" ++ f ++ " a ys) (" ++ g++ " a xs)))"

