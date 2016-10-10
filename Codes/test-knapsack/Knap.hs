module Knap where

import Data.List (union)

-- base bi-functor : F
-- F a b = 1 + a x b
-- fix (F a) = T a
data F a b = Nil | Cons a b deriving (Show,Eq)
newtype T a = InT (F a (T a)) deriving (Show,Eq)
-- Set
type Set a = [a]
-- from [a] to T a
toT :: Set a -> T a
toT [] = InT Nil
toT (x:xs) = InT $ Cons x (toT xs)
-- from T a to [a]
fromT :: T a -> Set a
fromT (InT Nil) = []
fromT (InT (Cons x xs)) = x : fromT xs

mapF :: (a -> c) -> (b -> d) -> F a b -> F c d
mapF f g Nil = Nil
mapF f g (Cons a b) = Cons (f a) (g b)

foldF :: (F a b -> b) -> T a -> b
foldF f (InT x) = f (mapF id (foldF f) x)

maxlist :: (a -> a -> Bool) -> Set a -> a
maxlist r = foldr1 max_r 
  where max_r a b = if a `r` b then b else a

thinlist :: (a -> a -> Bool) -> Set a -> Set a
thinlist q = foldr step []
  where step a []     = [a]
        step a (b:xs) | a `q` b = b : xs
                      | b `q` a = a : xs
                      | otherwise = b : step a xs

merge :: (a -> a -> Bool) -> (Set a,Set a) -> Set a
merge r ([],ys) = ys
merge r (xs,[]) = xs
merge r (a:xs,b:ys) | a `r` b = a : merge r (xs,b:ys)
                    | otherwise = b : merge r (a:xs,ys)

cpp :: (Set a,Set b) -> Set(a,b)
cpp (xs,ys) = [ (x,y) | x <- xs , y <- ys ]

cpr :: (a,Set b) -> Set(a,b)
cpr (x,ys) = [ (x,y) | y <- ys ]

wrap :: a -> Set a
wrap a = [a]

split :: (a -> b) -> (a -> c) -> a -> (b,c)
split f g x = ( f x , g x )
-------------------------------------------------
sumT :: T Int -> Int
sumT = foldF plusF
  where plusF :: F Int Int -> Int
        plusF Nil = 0
        plusF (Cons a b) = a + b
-------------------------------------------------
lengthT :: T a -> Int
lengthT = foldF lenF
  where lenF :: F a Int -> Int
        lenF Nil = 0
        lenF (Cons a b) = 1 + b
-------------------------------------------------
averageT :: T Int -> Int
averageT = ( \(s,l) -> s `div` l ) . foldF aveF
  where aveF :: F Int (Int,Int) -> (Int,Int)
        aveF Nil = (0,0)
        aveF (Cons a (b,n)) = ( a+b , n+1 )
-------------------------------------------------
subsequences :: T a -> Set (T a)
subsequences = foldF sbsqF
  where sbsqF Nil = wrap $ InT Nil
        sbsqF (Cons a xs) = [ InT (Cons a x) | x <- xs ] ++ xs
inits :: Eq a => T a -> Set (T a)
inits = foldF initF
  where initF Nil = [InT Nil]
        initF (Cons a xs) = [InT Nil] `union` (map (InT . (Cons a)) xs)
tails :: T a -> Set (T a)
tails = foldF tailF
  where tailF Nil = [InT Nil]
        tailF (Cons a (x:xs)) = (InT (Cons a x)) : x : xs
segments :: Eq a => T a -> Set (T a)
segments (InT Nil) = wrap $ InT Nil
segments (a@(InT (Cons x xs))) = inits a `union` (segments xs)
-------------------------------------------------
-- 0-1 knapsack problem
data Item = Item { value :: Int , weight :: Int  } deriving Show
items :: T Item
items = toT [ Item 10 5 , Item 40 5 , Item 30 6 , Item 50 4 ]

sumBothT :: T Item -> (Int,Int)
sumBothT = foldF f
  where f Nil = (0,0)
        f (Cons a b) = ( value a + fst b , weight a + snd b )
sumValT :: T Item -> Int
sumValT = fst . sumBothT
sumWtT :: T Item -> Int
sumWtT = snd . sumBothT
within :: Int -> T Item -> Bool
within w = \x -> sumWtT x < w

r :: T Item -> T Item -> Bool
r a b = sumValT a < sumValT b
q :: T Item -> T Item -> Bool
q a b = let (va,wa) = sumBothT a
            (vb,wb) = sumBothT b in
            wa == wb && va <= vb

-- ver. thinning
knap w = maxlist r . foldF f
  where f Nil = wrap $ InT Nil
        f (Cons a xs) =
          let tuple = ( [ InT (Cons a x) | x <- xs , within w (InT (Cons a x)) ] , xs )
          in thinlist q . merge r $ tuple

-------------------------------------------------

main :: IO()
main = do
  print $ fromT $ maxlist r $ filter (within 10) $ subsequences items
  print $ fromT $ knap 10 items
  print $ subsequences items
  print $ map fromT $ segments $ toT [1..5]
