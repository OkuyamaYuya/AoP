module Evaluation (evalFile,eval) where

import Base
import Lex
import Parse
import Syntax
import Data.Map as Map
import Debug.Trace

-- for debug : trace str -> a -> a
data VALUE = NN Int
           | BB Bool
           | LL [VALUE]
           | CLOSURE {env::ENV, var::String, e::Expr}
           | RECCLOS {env::ENV, f::String, var::String, e::Expr}
           | ERR String 
           deriving (Show)
type ENV   = Map String VALUE

evalFile = \s -> eval.parse.scanTokens <$> readFile s

eval = \s -> case s of
  Reject err -> ERR "syntax error"
  Accept e  -> eval_ e (Map.empty)

eval_ = \e -> \env -> case e of
  NAT n   -> NN n
  B   b   -> BB b
  VAR x   -> envLook (VAR x) env
  LIST (e1:rest) -> let v1 = eval_ e1 env in
                    case rest of
                      [] -> LL [v1]
                      _  -> mycons v1 $ eval_ (LIST rest) env
  GET e1 e2 -> let LL lst = eval_ e1 env in
               let NN n   = eval_ e2 env in
               lst!!n
  IF c t f-> case eval_ c env of
      BB True  -> eval_ t env
      BB False -> eval_ f env
  ABS x _ e -> CLOSURE env x e
  APP e1 e2 -> let v1 = eval_ e1 env in case v1 of
      CLOSURE env_ x e3 -> let v2 = eval_ e2 env 
                           in eval_ e3 (envAdd x v2 env_)
      RECCLOS env_ f x e3 -> let v2 = eval_ e2 env 
                             in eval_ e3 (envAdd f v1 . envAdd x v2 $ env_)
  REC f _ x e1 e2 -> eval_ e2 (envAdd f (RECCLOS env f x e1) env)
  BIND x _ e1 e2 -> let v1 = eval_ e1 env 
                    in eval_ e2 (envAdd x v1 env)
  PLUS  e1 e2 -> plus  (eval_ e1 env) (eval_ e2 env)
  MINUS e1 e2 -> minus (eval_ e1 env) (eval_ e2 env)
  TIMES e1 e2 -> times (eval_ e1 env) (eval_ e2 env)
  EQU   e1 e2 -> equ   (eval_ e1 env) (eval_ e2 env)
  AND   e1 e2 -> andd  (eval_ e1 env) (eval_ e2 env)
  OR    e1 e2 -> orr   (eval_ e1 env) (eval_ e2 env)

plus  v1 v2 = let (NN a,NN b) = (v1,v2) in NN $ a+b
minus v1 v2 = let (NN a,NN b) = (v1,v2) in NN $ a-b
times v1 v2 = let (NN a,NN b) = (v1,v2) in NN $ a*b
andd  v1 v2 = let (BB a,BB b) = (v1,v2) in BB $ a&&b
orr   v1 v2 = let (BB a,BB b) = (v1,v2) in BB $ a||b
equ   v1 v2 = case (v1,v2) of
                (NN a,NN b) -> BB $ a==b
                (BB a,BB b) -> BB $ a==b
mycons v1 l2 = let (LL rest) = l2 in LL $ v1 : rest

envLook :: Expr -> ENV -> VALUE
envLook (VAR str) env =
  case Map.lookup str env of
    Just a -> a
    _      -> ERR "not in scope"

envAdd :: String -> VALUE -> ENV -> ENV
envAdd x e env = Map.insert x e env

main :: IO()
main = do
  print $ eval.parse.scanTokens $ "[1,2,3][2]"
  print 0
