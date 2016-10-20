import DataStructure
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
within w = \x -> sumWtT x <= w

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

knap_naive1 = maxSet knapR . filter (within 10) . subsequences
knap_naive2 = solver_naive knapF (within 10) knapR 
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

items = toT [ Item 50 4, Item 3 12, Item 1 1 , Item 10 5 , Item 40 5 , Item 30 6 ,Item 100 2]
items2 = toT [ Item 10 5 , Item 40 5 , Item 30 5 , Item 50 5 , Item 100 5]
string1 = toT "todai"
string2 = toT "universityoftokyo"

-------------------------------------------------

main :: IO()
main = do
  let knap_funs = [ knap_naive1 , knap_naive2 , knap_thinning , knap_greedy ]
  putStrLn "knapsack problem"
  putStrLn "items"
  mapM_ (print.fromT) $ do
    fun <- knap_funs
    return $ fun items

  putStrLn "items2"
  mapM_ (print.fromT) $ do
    fun <- knap_funs
    return $ fun items2

  let lls_funs = [ lls_naive , lls_greedy , lls_thinning ]
  putStrLn "Lexicographically Largest subsequences"
  putStrLn "todai"
  mapM_ (print.fromT) $ do
    fun <- lls_funs
    return $ fun string1
  putStrLn "universityoftokyo"
  mapM_ (print.fromT) $ do
    fun <- lls_funs
    return $ fun string2
