module Main where

import Base
import Lex
import Parse
import Typecheck
import Syntax
-- import Evaluation
import System.Environment (getArgs)

-- ss = [ "b : Int = 1",
--        "p1 : Pair Int Int = (1,2)",
--        "f : Int->Int = \\a:Int.3",
--        "w : List Int = [1,2,3,4]", 
--        "f1 : Int -> (List Int) -> List Int = cons",
--        "sum : (List Int) -> Int = foldr plus 0",
--        "" ]

prettyPrint s = case s of
  CommentOut -> return ()
  BIND n t e -> do
    putStrLn $ "name\n\t" ++ show n
    putStrLn $ "type\n\t" ++ show t
    putStrLn $ "expr\n\t" ++ show e
    putStrLn "\n"

main::IO()
main = do
  s <- (head <$> getArgs) >>= readFile
  -- let s = unlines ss
  -- print "lex"
  -- print $ scanTokens $ s
  -- putStr "\n"
  putStrLn "--syntax--"
  let (Accept ( Program ls)) = parse.scanTokens $ s
  mapM_ prettyPrint ls
  -- putStr "\n"
  -- print "type"
  putStrLn "--type check--"
  let res_p = parse.scanTokens $ s in
    case res_p of
     Reject err -> putStrLn $ show err
     _ -> let res_t = tycheck res_p in
          case res_t of
            Reject err -> putStrLn err
            Accept _ -> putStrLn "OK"
            -- True -> let res_e = eval res_p in putStrLn $ show res_e


