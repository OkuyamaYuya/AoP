module Lexico where

import DataStructure
import Data.List (union)

-- Lexicographically Largest Subsequences

string1 = toT "todai"

-- global criterion
r = (<)
-- local criterion
q = (<)

lls_naive :: T Char -> T Char
lls_naive = maxSet (<) . subsequences

-- ver. thinning
lls_thinning = maxSet r . foldF f
  where f Nil = wrap $ InT Nil
        f (Cons a xss) =
          let tuple = ( [ InT (Cons a xs) | xs <- xss ] , xss )
          in thinSet q . merge r $ tuple

-- ver. greedy
lls_greedy = foldF f
  where f Nil = InT Nil
        f (y@(Cons a xs)) =
          let ss = [ InT y , xs ]
          in maxSet r ss

-------------------------------------------------

main :: IO()
main = do
  let funs = [ lls_naive, 
               lls_thinning, 
               lls_greedy]
  mapM_ (print.fromT) $ do
    fun <- funs
    return $ fun string1
