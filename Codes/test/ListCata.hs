-------------------------------------------------
-- cons-List
-- Catamorphism
-------------------------------------------------
{-# LANGUAGE ExistentialQuantification #-}
module ListCata where

import Data.List (union,nub)

data F a b = One | Cross a b deriving (Show,Eq,Ord)

newtype T a = InT (F a (T a)) deriving (Show,Eq,Ord)

nil :: F a (T a) -> T a
nil _ = InT One

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
toT [] = nil One
toT (x:xs) = cons $ Cross x (toT xs)

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

cppF :: F a (Set b) -> Set(F a b)
cppF One = wrap $ One
cppF (Cross a xss) = [ (Cross a xs) | xs <- xss ]

type Predicate a = a -> Bool

test :: (Predicate a) -> a -> Set a
test p x = if p x then [x] else []

-- singleton
wrap :: a -> Set a
wrap a = [a]


-------------------------------------------------
cpp :: (Set a,Set b) -> Set(a,b)
cpp (xs,ys) = [ (x,y) | x <- xs , y <- ys ]

cpr :: (a,Set b) -> Set(a,b)
cpr (x,ys) = [ (x,y) | y <- ys ]

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

-- input : ΛS , R , Q
-- S = [ f1 , f2 ]
-- 
-- filter embedding naive
--   max R . Λ (| S |)
-- = max R . (| Λ ( S . F ∈ ) |)
-- = max R . (| Λ S . Λ F ∈   |)
-- 
-- filter naive
-- max R . filter p . (| Λ S . Λ F ∈ |)
--
-- thinning
-- max R . (| thin Q . Λ S . Λ F ∈  |)
--
-- greedy
-- (| max R . Λ S  |)
-- 
-- S :: F a b -> b
-- Λ S :: F a b -> Set b
-- (| S |) :: T a -> b
-- Λ ( S . F ∈ ) :: F a (Set b) -> Set b

type Funs a b = ( [F a b -> Set b] , [F a b -> Set b] )

-- use constF to define sF
constF :: Funs a b -> Step a b
constF funs p x =
  case x of
    One       -> aux $ fst funs
    Cross _ _ -> aux $ snd funs
    where aux fs = do
            f <- fs
            f x


-- preorder on type a
type Order a = a -> a -> Bool

-- (| S |) :: T a -> b
type Step a b = Predicate b -> F a b -> Set b

-- Λ ( S . F ∈ )
powerS :: Eq b => (F a b -> Set b) -> F a (Set b) -> Set b
powerS sF = nub . concat . (mapSet sF) . cppF
-- nub : O (n^2) time

-- max R . (| thin Q . Λ ( S . F ∈ ) |)
solverThinning :: Eq b => Step a b -> Predicate b -> Order b -> Order b -> T a -> b
solverThinning gF p r q = maxSet r . foldF (thinSet q . powerS sF)
  where sF = gF p

-- (| max R . Λ S  |)
solverGreedy :: Eq b => Step a b -> Predicate b -> Order b -> T a -> b
solverGreedy gF p r = foldF ( maxSet r . sF )
  where sF = gF p

-- max R . filter p . (| Λ ( S . F ∈ ) |)
solverNaive :: Eq b => Step a b -> Predicate b -> Order b -> T a -> b
solverNaive gF p r = maxSet r . filter p . generator
  where 
    sF = gF (\x->True)
    generator = foldF ( powerS sF )

data Mode = Thinning | Greedy | Naive

solverMain :: Eq b => Step a b -> Predicate b -> Order b -> Order b -> Mode -> T a -> b
solverMain gF p r q mode =
  case mode of
    Thinning -> solverThinning gF p r q
    Greedy   -> solverGreedy gF p r
    Naive    -> solverNaive gF p r
-------------------------------------------------

subsequences :: Eq a => T a -> Set (T a)
subsequences = foldF ( powerS sF )
  where
    sF = constF (funs1,funs2) (\x->True)
    funs1 = [ wrap.nil ]
    funs2 = [ wrap.cons , wrap.outr ]

-- subsequences' :: Eq a => T a -> Set (T a)
-- subsequences' = foldF sbsqF
--   where
--     sbsqF One = wrap $ nil One
--     sbsqF (Cross a xss) = [ InT (Cross a xs) | xs <- xss ] `union` xss

inits :: Eq a => T a -> Set (T a)
inits = foldF ( powerS sF )
  where
    sF = constF (funs1,funs2) (\x->True)
    funs1 = [ wrap.nil ]
    funs2 = [ wrap.cons , wrap.nil ]

-- inits' :: Eq a => T a -> Set (T a)
-- inits' = foldF initF
--   where
--    initF One = wrap $ nil One
--    initF (Cross a xss) = (wrap $ nil One) `union` (map (cons . (Cross a)) xss)

tails' :: T a -> Set (T a)
tails' = foldF tailF
  where tailF One = wrap $ nil One
        tailF (Cross a (x:xs)) = (InT (Cross a x)) : x : xs

segments :: Eq a => T a -> Set (T a)
segments (InT One) = wrap $ nil One
segments (a@(InT (Cross x xs))) = inits a `union` (segments xs)
-------------------------------------------------
main = do
  print $ map fromT $ inits $ toT [1..3]
