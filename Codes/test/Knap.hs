module Knap where
import DataStructure
import Data.List (union)

-- 0-1 knapsack problem
data Item = Item { value :: Int , weight :: Int  } deriving (Show,Eq)
items :: T Item
items = toT [ Item 10 5 , Item 40 5 , Item 30 6 , Item 50 4 , Item 100 2]
items2 = toT [ Item 10 5 , Item 40 5 , Item 30 5 , Item 50 5 , Item 100 5]

sumBothT :: T Item -> (Int,Int)
sumBothT = foldF f
  where f Nil = (0,0)
        f (Cons a b) = ( value a + fst b , weight a + snd b )
sumValT :: T Item -> Int
sumValT = fst . sumBothT
sumWtT :: T Item -> Int
sumWtT = snd . sumBothT

-- predicate
within :: Int -> T Item -> Bool
within w = \x -> sumWtT x <= w

-- global criterion
r :: T Item -> T Item -> Bool
r a b = sumValT a < sumValT b
-- local criterion
q :: T Item -> T Item -> Bool
q a b = let (va,wa) = sumBothT a
            (vb,wb) = sumBothT b in
            wa == wb && va <= vb

-- ver. thinning
-- max r . (| thin q . s |)
knap_thinning :: Int -> T Item -> T Item
knap_thinning w = maxSet r . foldF f
  where f Nil = wrap $ InT Nil
        f (Cons a xs) =
          let tuple = ( [ InT (Cons a x) | x <- xs , within w (InT (Cons a x)) ] , xs )
          in thinSet q . merge r $ tuple

-- ver. fusion
-- max r . (| s |) where = (| s |) = filter (within w) . subsequences
knap_fusion :: Int -> T Item -> T Item
knap_fusion w = maxSet r . foldF sF
  where
    sF Nil = wrap $ InT Nil
    sF (Cons a xs) = [ InT (Cons a x) | x <- xs , within 10 (InT (Cons a x)) ] `union` xs

-- ver. naive
-- max r . filter (within w) . Λ (| subseq |) = max r . filter (within w) .subsequences
knap_naive :: Int -> T Item -> T Item
knap_naive w = maxSet r . filter (within w) . subsequences
-------------------------------------------------

main :: IO()
main = do
  let funs = [ knap_naive, 
               knap_thinning, 
               knap_fusion]
  putStrLn "items"
  mapM_ (print.fromT) $ do
    fun <- funs
    return $ fun 10 items
  putStrLn "items2"
  mapM_ (print.fromT) $ do
    fun <- funs
    return $ fun 10 items2
