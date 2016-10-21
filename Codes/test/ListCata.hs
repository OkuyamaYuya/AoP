-------------------------------------------------
-- cons-List
-- Catamorphism
-------------------------------------------------
{-# LANGUAGE ExistentialQuantification #-}
module ListCata where

import Data.List (union)

-- base bi-functor : F
-- F a b = 1 + a x b
-- fix (F a) = T a
data F a b = One | Cross a b deriving (Show,Eq,Ord)
newtype T a = InT (F a (T a)) deriving (Show,Eq,Ord)

nil :: T a
nil  = InT One
cons :: F a (T a) -> T a
cons (Cross a b) = InT (Cross a b)
outl :: F a b -> a
outl (Cross a b) = a
outr :: F a b -> b
outr (Cross a b) = b

-- Set
type Set a = [a]

-- from [a] to T a
toT :: Set a -> T a
toT [] = nil
toT (x:xs) = InT $ Cross x (toT xs)
-- from T a to [a]
fromT :: forall a . Show a => T a -> Set a
fromT (InT One) = []
fromT (InT x) = (outl x) : fromT (outr x)

mapF :: (a -> c) -> (b -> d) -> F a b -> F c d
mapF f g One = One
mapF f g (Cross a b) = Cross (f a) (g b)


mapSet :: (a -> b) -> Set a -> Set b
mapSet = map

-- cata-morphism
foldF :: (F a b -> b) -> T a -> b
foldF f (InT x) = f (mapF id (foldF f) x)

-- max function on Set
maxSet :: (a -> a -> Bool) -> Set a -> a
maxSet r = foldr1 max_r 
  where max_r a b = if a `r` b then b else a

-- thinning function on Set
thinSet :: (a -> a -> Bool) -> Set a -> Set a
thinSet q = foldr step []
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

cppF :: F a (Set b) -> Set(F a b)
cppF One = wrap $ One
cppF (Cross a xss) = [ (Cross a xs) | xs <- xss ]

type Predicate a = a -> Bool

test :: (Predicate a) -> a -> Set a
test p x = if p x then [x] else []

-- singleton
wrap :: a -> Set a
wrap a = [a]

split :: (a -> b) -> (a -> c) -> a -> (b,c)
split f g x = ( f x , g x )
-------------------------------------------------
sumT :: T Int -> Int
sumT = foldF plusF
  where plusF :: F Int Int -> Int
        plusF One = 0
        plusF (Cross a b) = a + b
-------------------------------------------------
lengthT :: T a -> Int
lengthT = foldF lenF
  where lenF :: F a Int -> Int
        lenF One = 0
        lenF (Cross a b) = 1 + b
-------------------------------------------------
averageT :: T Int -> Int
averageT = ( \(s,l) -> s `div` l ) . foldF aveF
  where aveF :: F Int (Int,Int) -> (Int,Int)
        aveF One = (0,0)
        aveF (Cross a (b,n)) = ( a+b , n+1 )
-------------------------------------------------

type Order a = a -> a -> Bool
type Step a b = Predicate b -> F a b -> Set b

-- Λ ( S . F ∈ )
powerS :: (F a b -> Set b) -> F a (Set b) -> Set b
powerS sF = concat . (mapSet sF) . cppF

-- max R . (| thin Q . Λ ( S . F ∈ ) |)
solverThinning :: Step a b -> Predicate b -> Order b -> Order b -> T a -> b
solverThinning gF p r q = maxSet r . foldF (thinSet q . powerS sF)
  where sF = gF p

-- (| max R . Λ S  |)
solverGreedy :: Step a b -> Predicate b -> Order b -> T a -> b
solverGreedy gF p r = foldF ( maxSet r . sF )
  where sF = gF p

-- max R . filter p . (| Λ ( S . F ∈ ) |)
solverNaive :: Step a b -> Predicate b -> Order b -> T a -> b
solverNaive gF p r = maxSet r . filter p . generator
  where 
    sF = gF (\x->True)
    generator = foldF ( powerS sF )

data Mode = Thinning | Greedy | Naive

solverMain :: Step a b -> Predicate b -> Order b -> Order b -> Mode -> T a -> b
solverMain gF p r q mode =
  case mode of
    Thinning -> solverThinning gF p r q
    Greedy   -> solverGreedy gF p r
    Naive    -> solverNaive gF p r
-------------------------------------------------

subsequences :: Eq a => T a -> Set (T a)
subsequences = foldF ( powerS sF )
  where sF One = wrap nil
        sF (c@(Cross a xs)) = (wrap $ cons c) `union` (wrap $ outr c)

subsequences' :: Eq a => T a -> Set (T a)
subsequences' = foldF sbsqF
  where sbsqF One = wrap nil
        sbsqF (Cross a xss) = [ InT (Cross a xs) | xs <- xss ] `union` xss

inits :: Eq a => T a -> Set (T a)
inits = foldF initF
  where initF One = wrap nil
        initF (Cross a xss) = (wrap nil) `union` (map (InT . (Cross a)) xss)

tails :: T a -> Set (T a)
tails = foldF tailF
  where tailF One = wrap nil
        tailF (Cross a (x:xs)) = (InT (Cross a x)) : x : xs

segments :: Eq a => T a -> Set (T a)
segments (InT One) = wrap $ nil
segments (a@(InT (Cross x xs))) = inits a `union` (segments xs)
-------------------------------------------------
