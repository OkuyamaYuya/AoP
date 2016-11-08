module Main where

import Base
import Lex
import Parse
import Typecheck
import Syntax
import TransZ3
import TransHs
import System.Process (system)
import System.Environment (getArgs)
import System.Exit

prettyPrint s = case s of
  CommentOut -> return ()
  BASETYPE bt -> putStrLn $ "BASETYPE\n\t" ++ showType bt ++ "\n"
  RIGHT   fs -> putStrLn $ "RIGHT\n\t" ++ show fs ++ "\n"
  LEFT    fs -> putStrLn $ "LEFT\n\t" ++ show fs ++ "\n"
  BIND n as t e -> do
    putStrLn $ "name\n\t" ++ show n
    putStrLn $ "args\n\t" ++ show as
    putStrLn $ "type\n\t" ++ show t
    putStrLn $ "expr\n\t" ++ show e
    putStrLn "\n"

z3file = "./temp/test.z3"
hsfile = "./temp/thin_or_greedy.hs"

main::IO()
main = do
  s <- (head <$> getArgs) >>= readFile

  putStrLn "--syntax--"
  let (Accept (Program ls)) = parse.scanTokens $ s
  mapM_ prettyPrint ls

  -- type check & generate
  let resultParse = parse.scanTokens $ s in
    case resultParse of
     Reject err -> putStrLn $ "syntax error\n" ++ show err
     _ -> let resultTyCheck = tycheck resultParse in
          case resultTyCheck of
            Reject err -> putStrLn $ "type error\n" ++ err
            Accept _ -> do
              putStrLn "--type check--\nOK"
              putStrLn "--z3 code--"
              let resultTransZ3 = transZ3 resultParse
              putStrLn resultTransZ3
              writeFile z3file resultTransZ3
              -- execute monotoneCheck.sh
              putStrLn "--check monotonicity--"
              monotoneOrNot <- system "./monotoneCheck.sh"
              case monotoneOrNot of
                ExitSuccess -> do
                  let resultTransHs = transHs resultParse
                  putStrLn resultTransHs
                  writeFile hsfile resultTransHs
                _ -> putStrLn "Failure"
