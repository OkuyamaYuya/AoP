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
  BASETYPE x -> putStrLn $ "BASETYPE\n\t" ++ showType x ++ "\n"
  INPUT x -> putStrLn $ "INPUT\n\t" ++ show x ++ "\n"
  RIGHT x -> putStrLn $ "RIGHT\n\t" ++ show x ++ "\n"
  LEFT  x -> putStrLn $ "LEFT\n\t" ++ show x ++ "\n"
  BIND n as t e -> do
    putStrLn $ "name\n\t" ++ show n
    putStrLn $ "args\n\t" ++ show as
    putStrLn $ "type\n\t" ++ show t
    putStrLn $ "expr\n\t" ++ show e
    putStrLn "\n"
  CommentOut -> return ()

hsfile = "./listcata/thinSolver.hs"

main::IO()
main = do
  f <- (head <$> getArgs) >>= readFile
  -- type check & generate
  let resultParse = parse.scanTokens $ f in
    case resultParse of
     Reject err -> putStrLn $ "syntax error\n" ++ show err
     Accept (Program ls) -> do
        putStrLn "-- syntax --"
        mapM_ prettyPrint ls
        let resultTyCheck = tycheck resultParse
        case resultTyCheck of
          Reject err -> putStrLn $ "type error\n" ++ err
          Accept _ -> do
            putStrLn "-- type check --\nOK"
            putStrLn "-- z3 code --"
            resultTransZ3 <- transZ3 resultParse
            putStrLn resultTransZ3
            putStrLn "-- check monotonicity --"
            monotoneOrNot <- system ("./monotoneCheck.sh " ++ z3Monotone)
            case monotoneOrNot of
              ExitSuccess -> do
                resultTransHs <- transHs resultParse
                putStrLn "-- Haskell code --"
                putStrLn resultTransHs
                writeFile hsfile resultTransHs
                putStrLn "-- run Haskell --"
                resultRun <- system "./runThinSolver.sh"
                return ()
              _ -> putStrLn "Failure"


