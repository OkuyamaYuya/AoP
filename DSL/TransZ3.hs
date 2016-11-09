module TransZ3 ( transZ3,
                 getInfo,
                 z3Monotone,
                 z3Connected
               ) where

import Base
import Lex
import Parse
import Syntax
import Data.Map as Map
import Data.List (isInfixOf)
import Debug.Trace

z3Monotone = "./temp/testMonotone.z3"
z3Connected = "./temp/testConnected.z3"

transZ3 :: Result Program -> IO String
transZ3 prog = case prog of
  Reject err -> return err
  Accept (Program ss) ->
    case getInfo ss of
      Reject err -> return err
      Accept (_,rr,bb,lx) -> do
        let template = header bb lx ++ (unlines $ fmap transZ3_ ss)
        writeFile z3Monotone (template ++ makeQuery rr bb)
        writeFile z3Connected (template ++ makeQuery2 bb)
        return template

type LexicoUsed = Bool

getInfo :: [Sentence] -> Result ([String],[String],TY,LexicoUsed)
getInfo ss =
  let ll = findLeft ss
      rr = findRight ss
      bb = findBase ss
      lx = findLexico ss
  in
    case (ll,rr,bb,lx) of
      (Nothing,_,_,_) -> 
        Reject "write 'LEFT'."
      (_,Nothing,_,_) -> 
        Reject "write 'RIGHT'."
      (_,_,Nothing,_) -> 
        Reject "write 'BASETYPE'."
      (Just jfs,Just jes,Just jbt,_) -> 
        Accept (fmap showExpr jfs,fmap showExpr jes,jbt,lx)
  where
    -- find leftmost one.
    findRight [] = Nothing
    findRight ((RIGHT x):xs) = Just x
    findRight (_:xs) = findRight xs
    findLeft [] = Nothing
    findLeft ((LEFT x):xs) = Just x
    findLeft (_:xs) = findLeft xs
    findBase [] = Nothing
    findBase ((BASETYPE x):xs) = Just x
    findBase (_:xs) = findBase xs
    findLexico [] = False
    findLexico ((BIND _ _ _ e):xs)
      | isInfixOf "leq_lexico" (show e) = True
      | otherwise = findLexico xs
    findLexico (_:xs) = findLexico xs

transZ3_ (LEFT _) = ""
transZ3_ (RIGHT _) = ""
transZ3_ (BASETYPE _) = ""
transZ3_ CommentOut = ""
transZ3_ (BIND varName varArgs varType varExpr) = case varExpr of
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
  where aux x t = "(" ++ x ++ " " ++ t ++ ")"
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
header :: TY -> LexicoUsed -> String
header bt lx = unlines $ [ 
    "(declare-datatypes (T1 T2) ((Pair (mk-pair (fst T1) (snd T2)))))",
    "(define-fun cons ((x "++showType bt++") (xs (List "++showType bt++"))) (List "++showType bt++")",
    "  (insert x xs))",
    "(define-fun outr ((x "++showType bt++") (xs (List "++showType bt++"))) (List "++showType bt++")",
    "  xs)",
    "(declare-fun leq (Int Int) Bool)",
    "(assert (forall ((x Int) (y Int)) (= (<= x y) (leq x y))))" ] ++
      if lx then [
    "(declare-fun leq_lexico ((List Int) (List Int)) Bool)",
    "(assert (forall ((xs (List Int)) (ys (List Int)))",
    "  (= (leq_lexico xs ys)",
    "  (or",
    "    (= xs nil)",
    "    (< (head xs) (head ys))",
    "    (and (= (head xs) (head ys))",
    "         (leq_lexico (tail xs) (tail ys)))))))" ] else [""]


makeQuery :: [String] -> TY -> String
makeQuery fs bt = "(push)\n" ++ comment ++ 
                  a1 ++ a2 ++ a3 ++ 
                  "\n(check-sat)\n(pop)"
  where
    comment = "(echo \"Is q monotonic ?\")\n"
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
    unlines $ [
    "(assert (= " ++ bf,
    "    (forall ((xs (List " ++ bb ++ ")) (ys (List " ++ 
                  bb ++ ")) (a " ++ bb ++ "))",
    "    (=> (q ys xs) ",
    "    (=> (p (" ++ f ++ " a ys))",
    "        (or "] ++ fmap (target f) fs ++ ["))))))"]
      where
        bb = showType btype
        target f g = "\t(and (p (" ++ g ++ " a xs)) (q (" ++ f ++ " a ys) (" ++ g++ " a xs)))"

makeQuery2 btype = unlines [
      "\n(push)\n(echo \"Is q connected ?\")",
      "(assert (not",
      "    (forall ((x (List " ++ bb ++ ")) (y (List " ++ bb ++ ")))",
      "          (or (q x y) (q y x)))))",
      "(check-sat)\n(pop)" ]
  where
    bb = showType btype

