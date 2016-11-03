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
       "let w : List Int = [1,2,3,4]", 
       "let f1 : Int -> (List Int) -> List Int = cons",
       "let sum : (List Int) -> Int = foldr plus 0",
       "" ]
main::IO()
main = do
  s <- (head <$> getArgs) >>= readFile
  -- let s = unlines ss
  print "lex"
  print $ scanTokens $ s
  putStr "\n"
  print "syntax"
  print $ parse.scanTokens $ s
  putStr "\n"
  print "type"
  let res_p = parse.scanTokens $ s in
    case res_p of
     Reject err -> putStrLn $ show err
     _ -> let res_t = tycheck res_p in
          case res_t of
            Reject err -> putStrLn err
            Accept a -> print a
            -- True -> let res_e = eval res_p in putStrLn $ show res_e


