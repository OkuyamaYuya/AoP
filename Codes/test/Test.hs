{-# LANGUAGE ExistentialQuantification #-}
import ListCata
import Data.List (union)

-------------------------------------------------
-- 0-1 knapsack problem
-------------------------------------------------
data Item = Item { value :: Int , weight :: Int  } deriving (Show,Eq)

sumBothT :: T Item -> (Int,Int)
sumBothT = foldF f
  where f One = (0,0)
        f (Cross a b) = ( value a + fst b , weight a + snd b )

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


knapF :: Predicate (T Item) -> Step Item (T Item)
knapF p = constF (funs1,funs2)
  where
    funs1 = [ Just . nil ]
    funs2 = [ Just . outr , test p . cons ]
-- knapF p One = wrap $ nil One
-- knapF p x = (wrap $ outr x) `union` (test p $ cons x)


knapMain = solverMain knapF (within 10) knapR knapQ
knapNaive = knapMain Naive
knapThinning = knapMain Thinning
knapGreedy = knapMain Greedy

-------------------------------------------------
-- Lexicographically Largest Subsequences
-------------------------------------------------

-- global criterion
llsR :: Order (T Char)
llsR = (<=)
-- local criterion
llsQ :: Order (T Char)
llsQ = (<=)

llsF :: Ord a => Predicate (T a) -> Step a (T a)
llsF _ = constF (funs1,funs2)
  where
    funs1 = [ Just . nil ]
    funs2 = [ Just . outr , Just . cons ]
-- llsF p One = wrap $ nil One
-- llsF p x = (wrap $ outr x) `union` (wrap $ cons x)

llsMain = solverMain llsF (\x->True) llsR llsQ
llsNaive = llsMain Naive
llsGreedy = llsMain Greedy
llsThinning = llsMain Thinning

-------------------------------------------------
-- test case
-------------------------------------------------

items1 = toT [ Item 50 4, Item 3 12, Item 1 1 ,  Item 10 5,
               Item 40 5, Item 30 6, Item 100 2, Item 3 4,
               Item 4 53, Item 4 2 , Item 32 3 , Item 3 2 ]
items2 = toT [ Item 10 5 , Item 40 5 , Item 30 5 , Item 50 5 , Item 100 5]
string1 = toT "todai"
string2 = toT "universityoftokyozzzzzzzz"


knap_funs = [ knapNaive , knapThinning , knapGreedy ]
itemss    = [ items1 , items2 ]

lls_funs = [ llsThinning , llsGreedy ]
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


