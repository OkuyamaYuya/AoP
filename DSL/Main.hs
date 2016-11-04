module Main where

import Base
import Lex
import Parse
-- import Typecheck
import Syntax
-- import Evaluation
import System.Environment (getArgs)

prettyPrint s = case s of
  CommentOut -> return ()
  BIND n as t e -> do
    putStrLn $ "name\n\t" ++ show n
    putStrLn $ "args\n\t" ++ show as
    putStrLn $ "type\n\t" ++ show t
    putStrLn $ "expr\n\t" ++ show e
    putStrLn "\n"

main::IO()
main = do
  -- s <- (head <$> getArgs) >>= readFile
  let s = "f x : Int->Int = x + 100"
  putStrLn "--syntax--"
  let (Accept ( Program ls)) = parse.scanTokens $ s
  mapM_ prettyPrint ls

  -- type check & generate
  -- let resultParse = parse.scanTokens $ s in
  --   case resultParse of
  --    Reject err -> putStrLn $ "syntax error\n" ++ show err
  --    _ -> let resultTyCheck = tycheck resultParse in
  --         case resultTyCheck of
  --           Reject err -> putStrLn $ "type error\n" ++ err
  --           Accept _ -> do
  --             putStrLn "--type check--\nOK"
  --             putStrLn "--z3 code--"
  --             let resultEval = eval resultParse
  --             putStrLn resultEval
  --             writeFile "./test.z3" resultEval


