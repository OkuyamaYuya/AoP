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
  Accept (Program ss) -> unlines $ fmap eval_ ss

eval_ CommentOut = ""
eval_ (BIND varName varType varExpr) = case varExpr of
  NAT n -> (declareConst varName varType n)
  _ -> ""

main :: IO()
main = do
  putStrLn $ eval.parse.scanTokens $ "x : Int = 10"
  print 0

declareConst x typ num = a1 ++ "\n" ++ a2
  where
    a1 = "(declare-const "++show x++" "++show typ++")"
    a2 = "(assert (= "++show x++" "++show num++"))"
