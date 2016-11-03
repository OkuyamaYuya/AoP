module Main where

import Base
import Lex
import Parse
import Typecheck
import Syntax
-- import Evaluation
import System.Environment (getArgs)

ss = [ "let b : Int = 1",
       "let p1 : Pair Int Int = (1,2)",
       "let f : Int->Int = \\a:Int.3",
       "let z : Int = if 1 == 1 then 2 else 4",
       "let w : List Int = [1,2,3,4]", 
       -- "let sum : (List Int) -> Int = foldr plus 0",
       -- "let f1 : (Pair Int Int)->Int = cons",
       "" ]

s = unlines ss

main::IO()
main = do
  print $ parse.scanTokens $ s
  -- s <- (head <$> getArgs) >>= readFile
  let res_p = parse.scanTokens $ s in
    case res_p of
     Reject err -> putStrLn $ show err
     _ -> let res_t = tycheck res_p in
          case res_t of
            Nothing -> putStrLn "err"
            Just _  -> print res_t
            -- True -> let res_e = eval res_p in putStrLn $ show res_e

