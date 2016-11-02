module Typecheck (tycheckFile,tycheck) where

import Syntax
import Parse
import Lex
import Base
import Data.Map as Map

import Debug.Trace

type ENV_ty = Map String TY

tycheckFile = \s -> tycheck.parse.scanTokens <$> readFile s

tycheck = \s -> case s of
  Reject err -> BOTTOM "syntax error"
  Accept e  -> tycheck_ e (Map.empty)

tycheck_ = \e -> \env -> case e of
  NAT n -> INT
  B   b -> BOOL
  VAR x -> envLook (VAR x) env
  LIST (e1:rest) -> let t1 = tycheck_ e1 env in
                    case rest of
                      [] -> LISTtype t1
                      _  -> if LISTtype t1 == tycheck_ (LIST rest) env
                            then LISTtype t1 
                            else BOTTOM "List type error"
  GET e1 e2   -> let t1 = tycheck_ e1 env in
                 if (tycheck_ e2 env) /= INT then BOTTOM "type error : list[i], i must be Int type."
                 else 
                    case t1 of
                      LISTtype a -> a
                      _ -> BOTTOM "type error : x[i] , x must be LIST."
  IF e1 e2 e3 -> let t1 = tycheck_ e1 env in
                 let t2 = tycheck_ e2 env in
                 let t3 = tycheck_ e3 env in
                 if t1==BOOL && t2==t3 then t2 else BOTTOM "type error in if statement"
  BIND x t e1 e2 -> let t1 = tycheck_ e1 env in tycheck_ e2 (envAdd x t1 env)
  ABS x t1 e -> let t2 = tycheck_ e (envAdd x t1 env) in FUN t1 t2
  REC f t x e1 e2 -> case t of
                      FUN dom cod -> let t1 = tycheck_ e1 (envAdd f t . envAdd x dom $ env) in
                                     let t2 = tycheck_ e2 (envAdd f t env) in
                                     if t1 == cod then t2 else BOTTOM "REC : codomain"
  APP e1 e2  -> let t = tycheck_ e1 env in
                  case t of
                    FUN t1 t2 -> let t3 = tycheck_ e2 env in
                                  if t1 == t3 then t2 else BOTTOM "apply e1 e2 : types not match"
                    _ -> BOTTOM "apply e1 e2 : types not match"
  PLUS  e1 e2 -> if (tycheck_ e1 env,tycheck_ e2 env) == (INT,INT) 
                  then INT 
                  else BOTTOM "(+)::INT->INT->INT"
  MINUS e1 e2 -> if (tycheck_ e1 env,tycheck_ e2 env) == (INT,INT) 
                  then INT 
                  else BOTTOM "(-)::INT->INT->INT"
  TIMES e1 e2 -> if (tycheck_ e1 env,tycheck_ e2 env) == (INT,INT) 
                  then INT 
                  else BOTTOM "(*)::INT->INT->INT"
  EQU e1 e2  ->   let t1 = tycheck_ e1 env in
                  let t2 = tycheck_ e2 env in
                  if t1 == t2
                  then BOOL
                  else BOTTOM "(==)::a->a->BOOL"
  AND e1 e2  -> let t1 = tycheck_ e1 env in
                  if t1 == (tycheck_ e2 env) && t1 == BOOL 
                    then BOOL 
                    else BOTTOM "(&&)::BOOL->BOOL->BOOL"
  OR  e1 e2  -> let t1 = tycheck_ e1 env in
                  if t1 == (tycheck_ e2 env) && t1 == BOOL 
                    then BOOL 
                    else BOTTOM "(||)::BOOL->BOOL->BOOL"
  _ -> BOTTOM "error"

envLook :: EXP -> ENV_ty -> TY
envLook (VAR str) env =
  case Map.lookup str env of
    Just a -> a
    _      -> BOTTOM "not in scope"

envAdd :: String -> TY -> ENV_ty -> ENV_ty
envAdd x e env = Map.insert x e env

main :: IO()
main = do
  print $ tycheck.parse.scanTokens $ "let rec f n : Int -> List Int = if n == 0 then [0,0,0,0] else [1,2,3,4] in (f 3)[2]"
  print 0
