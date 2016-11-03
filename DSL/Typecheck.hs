module Typecheck (tycheckFile,tycheck) where

import Syntax
import Parse
import Lex
import Base
import Data.Map as Map

import Debug.Trace

type ENV_ty = Map String TY

default_env = Map.empty

tycheckFile s = tycheck.parse.scanTokens <$> readFile s

tycheck prog = case prog of
  Reject err -> Nothing
  Accept ps  -> let Program ss = ps
                in Prelude.foldl aux (Just default_env) ss
                where
                  aux Nothing _ = Nothing
                  aux (Just env) s =
                    let (BIND x t e) = s in
                    if t == tycheck_ e env
                    then Just (envAdd x t env)
                    else Nothing

tycheck_ e env = case e of
  NAT n -> INT
  B   b -> BOOL
  VAR x -> envLook (VAR x) env
  PAIR x y -> PAIRty (tycheck_ x env) (tycheck_ y env)
  LIST (e1:rest) -> let t1 = tycheck_ e1 env in
                    case rest of
                      [] -> LISTty t1
                      _  -> if LISTty t1 == tycheck_ (LIST rest) env
                            then LISTty t1 
                            else BOTTOM "List type error"
  GET e1 e2   -> let t1 = tycheck_ e1 env in
                 if (tycheck_ e2 env) /= INT then BOTTOM "type error : list[i], i must be Int type."
                 else 
                    case t1 of
                      LISTty a -> a
                      _ -> BOTTOM "type error : x[i] , x must be LIST."
  IF e1 e2 e3 -> let t1 = tycheck_ e1 env in
                 let t2 = tycheck_ e2 env in
                 let t3 = tycheck_ e3 env in
                 if t1==BOOL && t2==t3 then t2 else BOTTOM "type error in if statement"
  ABS x t1 e -> let t2 = tycheck_ e (envAdd x t1 env) in FUN t1 t2
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

envLook :: Expr -> ENV_ty -> TY
envLook (VAR str) env =
  case Map.lookup str env of
    Just a -> a
    _      -> BOTTOM "not in scope"

envAdd :: String -> TY -> ENV_ty -> ENV_ty
envAdd x e env = Map.insert x e env

-- main :: IO()
-- main = do
--   print $ tycheck.parse.scanTokens $ "let f : Int -> (List Int) = \\n : Int. if n == 0 then [0,0,0,0] else [1,2,3,4]"
--   print $ tycheck.parse.scanTokens $ "let y : Int -> Int = \\a:Int.3"
