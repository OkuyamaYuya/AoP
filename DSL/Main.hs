module Main where

import Base
import Lex
import Parse
import Typecheck
import Syntax
import Evaluation
import System.Environment (getArgs)

main::IO()
main = do
  s <- (head <$> getArgs) >>= readFile
  let res_p = parse.scanTokens $ s in
    case res_p of
     Reject err -> putStrLn $ show err
     _ -> let res_t = tycheck res_p in
          case res_t of
            BOTTOM err -> putStrLn err
            _ -> let res_e = eval res_p in putStrLn $ show res_e
