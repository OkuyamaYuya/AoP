{-# LANGUAGE ExistentialQuantification #-}
import ListCata
import Data.List (union)

-- 0-1 knapsack problem

data Item = Item { value :: Int , weight :: Int  } deriving (Show,Eq)

sumBothT :: T Item -> (Int,Int)
sumBothT = foldF f
  where f Nil = (0,0)
        f (Cons a b) = ( value a + fst b , weight a + snd b )
sumValT :: T Item -> Int
sumValT = fst . sumBothT
sumWtT :: T Item -> Int
sumWtT = snd . sumBothT

-- predicate
within :: Int -> Predicate (T Item)
within w = \x -> (sumWtT x <= w)

-- global criterion
knapR :: T Item -> T Item -> Bool
knapR a b = sumValT a <= sumValT b
-- local criterion
knapQ :: T Item -> T Item -> Bool
knapQ a b = let (va,wa) = sumBothT a
                (vb,wb) = sumBothT b in
                wa == wb && va <= vb

knapF :: Step Item (T Item)
knapF p Nil = wrap $ InT Nil
knapF p (c@(Cons a xs)) = (wrap xs) `union` (test p (InT c))

knap_naive = solver_naive knapF (within 10) knapR 
knap_thinning = solver_thinning knapF (within 10) knapR knapQ
knap_greedy = solver_greedy knapF (within 10) knapR



-- Lexicographically Largest Subsequences

-- global criterion
llsR :: Order (T Char)
llsR = (<=)
-- local criterion
llsQ :: Order (T Char)
llsQ = (<=)

llsF :: Ord a => Step a (T a)
llsF p Nil = wrap $ InT Nil
llsF p (c@(Cons a xs)) = (wrap xs) `union` (wrap $ InT c)

lls_naive = solver_naive llsF (\x->True) llsR
lls_greedy = solver_greedy llsF (\x->True) llsR
lls_thinning = solver_thinning llsF (\x->True) llsR llsQ

-------------------------------------------------
-- test case

items1 = toT [ Item 50 4, Item 3 12, Item 1 1, Item 10 5,
               Item 40 5, Item 30 6, Item 100 2, Item 3 4,
               Item 4 53, Item 4 2, Item 32 3, Item 3 2 ]
items2 = toT [ Item 10 5 , Item 40 5 , Item 30 5 , Item 50 5 , Item 100 5]
string1 = toT "todai"
string2 = toT "universityoftokyozzzzzzzz"

knap_funs = [ knap_naive , knap_thinning , knap_greedy ]
itemss    = [ items1 , items2 ]

lls_funs = [ lls_greedy , lls_thinning ]
strings  = [ string1 , string2 ]

fff (funs,inputs) =
  mapM_ (print.fromT) $ do
    input <- inputs
    fun <- funs
    return $ fun input
-------------------------------------------------

main :: IO()
main = do
  fff (knap_funs,itemss)
  fff (lls_funs,strings)
