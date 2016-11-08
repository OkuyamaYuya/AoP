module Typecheck (tycheckFile,tycheck) where

import Syntax
import Parse
import Lex
import Base
import Data.Map as Map

import Debug.Trace

type ENV_ty = Map String TY

------------------------
fa a b = FUN a (FUN b b)
default_env a = fromList[("nil"  ,(LISTty a)),
                         ("cons" ,fa a (LISTty a)),
                         ("outr" ,fa a (LISTty a)),
                         ("plus" ,fa INT INT),
                         ("fst"  ,FUN (PAIRty INT INT) INT),
                         ("snd"  ,FUN (PAIRty INT INT) INT),
                         ("leq"  ,FUN INT (FUN INT BOOL)),
                         ("leq_lexico" ,FUN (LISTty INT) (FUN (LISTty INT) BOOL)),
                         (""     ,BOTTOM "")]
------------------------

tycheckFile s = tycheck.parse.scanTokens <$> readFile s

tycheck prog = case prog of
  Reject err -> Reject err
  Accept (Program ss) -> case (lookupBtype ss) of
    Nothing -> Reject "You have to write BASETYPE."
    Just basetype ->
      Prelude.foldl aux (Accept $ default_env basetype) ss
        where
          aux (Reject a) _ = Reject a
          aux (Accept env) s = case s of
            BIND x as t e ->
              case (mkEnv as t env) of
                Reject err -> Reject $ "err in " ++ x ++ " :\n" ++ err
                Accept envMk ->
                  let this_type = tycheck_ e envMk
                      cod x [] = x
                      cod (FUN x xs) (a:funArgs) = cod xs funArgs
                  in if cod t as == this_type
                        then Accept (envAdd x t env)
                        else Reject (show e++" "++show this_type++
                                     " doesn't matches "++
                                     show t++" in "++show s)
            _ -> Accept env

lookupBtype [] = Nothing
lookupBtype ((BASETYPE b):_) = Just b
lookupBtype (_:xs) = lookupBtype xs


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
                 if (tycheck_ e2 env) /= INT 
                    then BOTTOM "type error : list[i], i must be Int type."
                    else 
                      case t1 of
                        LISTty a -> a
                        _ -> BOTTOM "type error : x[i] , x must be LIST."
  IF e1 e2 e3 -> let t1 = tycheck_ e1 env
                     t2 = tycheck_ e2 env
                     t3 = tycheck_ e3 env in
                 if t1==BOOL && t2==t3 
                    then t2 
                    else BOTTOM "type error in if statement"
  FOLDR f e ->  case (tycheck_ f env) of
                  FUN a (FUN b c) -- f :: a -> b -> b , e :: b
                    | b == (tycheck_ e env) && b == c -> FUN (LISTty a) b
                    | otherwise -> BOTTOM $ "e's type in fold isn't correct."++(show b)
                  _ -> BOTTOM "f's type is not correct."
  APP e1 e2  -> let t = tycheck_ e1 env in
                  case t of
                    FUN t1 t2 -> let t3 = tycheck_ e2 env in
                                  if t1 == t3
                                     then t2 
                                     else BOTTOM "apply e1 e2 : types not match"
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
  EQU e1 e2  ->   let t1 = tycheck_ e1 env
                      t2 = tycheck_ e2 env in
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

-- f x y : a -> b -> c = x + y
mkEnv :: [String] -> TY -> ENV_ty -> Result ENV_ty
mkEnv []      funType env = Accept env
mkEnv funArgs funType env =
  let newEnv = (fromList $ zip funArgs (types funType))
      intersect = intersection newEnv env
  in if intersect == Map.empty then Accept $ union newEnv env
     else Reject $ "argument " ++ (fst.head.toList) intersect ++ " isn't valid."
  where
    types (FUN a b) = a : types b
    types _         = []

envLook :: Expr -> ENV_ty -> TY
envLook (VAR str) env =
  case Map.lookup str env of
    Just a -> a
    _      -> BOTTOM "not in scope"

envAdd :: String -> TY -> ENV_ty -> ENV_ty
envAdd x e env = Map.insert x e env

-- main = do
  -- print $ tycheck.parse.scanTokens $ "BASETYPE:Int\ne1 : (List Int) = nil"
