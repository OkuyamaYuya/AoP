-------------------------------------------------
-- cons-List
-- Catamorphism
-------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module ListCata where

import Data.List (union,nub)
import Data.Maybe (maybeToList)

-------------------------------------------------

data M f a = In (f a (M f a))

data (f :+: g) a b = Inl (f a b) | Inr (g a b) deriving (Eq,Ord)

-------------------------------------------------

instance (Show a,Show2 f) => Show (M f a) where
  show = show2'
    where
      show2' (In x) = show2 x

instance (Eq a,Eq2 f) => Eq (M f a) where
  (==) = eq2'
    where
      eq2' (In x) (In y) = x `eq2` y

instance (Ord a,Ord2 f) => Ord (M f a) where
  compare = compare2'
    where
      compare2' (In x) (In y) = compare2 x y

instance (Functor (f a),Functor (g a)) => Functor ((f :+: g) a) where
  fmap f (Inl x) = Inl (fmap f x)
  fmap f (Inr x) = Inr (fmap f x)

-- instance (Show (f a b),Show (g a b)) => Show ((f :+: g) a b) where
--   show (Inl x) = "Inl " ++ show x
--   show (Inr x) = "Inr " ++ show x

-------------------------------------------------

class Show2 f where
  show2 :: (Show a,Show2 g) => (f a (M g a)) -> String

instance (Show2 f,Show2 g) => Show2 (f :+: g) where
  show2 (Inl x) = show2 x
  show2 (Inr x) = show2 x

class Eq2 f where
  eq2 :: (Eq a,Eq2 g) => (f a (M g a)) -> (f a (M g a)) -> Bool

instance (Eq2 f,Eq2 g) => Eq2 (f :+: g) where
  (Inl x) `eq2` (Inl y) = x `eq2` y
  (Inr x) `eq2` (Inr y) = x `eq2` y
  (Inl x) `eq2` (Inr y) = False
  (Inr x) `eq2` (Inl y) = False

class Eq2 f => Ord2 f where
  compare2 :: (Ord a,Ord2 g) => (f a (M g a)) -> (f a (M g a)) -> Ordering

instance (Ord2 f,Ord2 g) => Ord2 (f :+: g) where
  compare2 (Inl x) (Inl y) = compare2 x y
  compare2 (Inr x) (Inr y) = compare2 x y
  compare2 (Inl x) (Inr y) = LT
  compare2 (Inr x) (Inl y) = GT

-------------------------------------------------
-- cons List
-------------------------------------------------

data One a b  = One deriving (Show,Eq,Ord)
data Cross a b = Cross a b deriving (Show,Eq,Ord)

instance Functor (One a) where
  fmap f One = One
instance Functor (Cross a) where
  fmap f (Cross x y) = Cross x (f y)

instance Show2 One where
  show2 One = "[]"
instance Show2 Cross where
  show2 (Cross x xs) = show x ++ ":" ++ show2' xs
    where
      show2' (In x) = show2 x

instance Eq2 One where
  One `eq2` One = True
instance Eq2 Cross where
  (Cross x xs) `eq2` (Cross y ys) = x == y && xs `eq2'` ys
    where
      eq2' (In x) (In y) = x `eq2` y

instance Ord2 One where
  compare2 One One = EQ

instance Ord2 Cross where
  compare2 (Cross x xs) (Cross y ys) =
    case (compare x y) of
      GT -> GT
      LT -> LT
      EQ -> compare2' xs ys
        where 
          compare2' (In a) (In b) = compare2 a b

type L = One :+: Cross
type List a = M L a

-------------------------------------------------

foldF :: Functor (f a) => (f a b -> b) -> (M f a) -> b
foldF f (In x) = f ( fmap (foldF f) x )

toList :: [a] -> List a
toList [] = In (Inl One)
toList (x:xs) = In (Inr (Cross x (toList xs)))

fromList :: List a -> [a]
fromList (In (Inl One)) = []
fromList (In (Inr (Cross x xs))) = x : fromList xs

-- Set
type Set a = [a]

mapSet :: (a -> b) -> Set a -> Set b
mapSet = map

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

type Predicate a = a -> Bool

test :: (Predicate a) -> a -> Maybe a
test p x = if p x then Just x else Nothing

-- singleton
wrap :: a -> Set a
wrap x = [x]

-- F ∈
cppL :: L a (Set b) -> Set (L a b)
cppL (Inl One) = wrap $ Inl One
cppL (Inr (Cross x ys)) = [ Inr (Cross x y) | y <- ys ]

-------------------------------------------------

type Order a = a -> a -> Bool
type MaybeFalg a b = L a b -> Maybe b
type Funs a b = ([MaybeFalg a b],[MaybeFalg a b])


-- powerF S = Λ S
powerF :: Funs a b -> L a b -> Set b
powerF funs x =
  case x of
    Inl _ -> aux $ fst funs
    Inr _ -> aux $ snd funs
    where aux fs = do
            f <- fs
            maybeToList $ f x

-- Λ ( S . F ∈ ) = E S . Λ F ∈
-- nub : O (n^2) time
mapE :: Eq b => Funs a b -> Set(L a b) -> Set b
mapE funs set = nub $ aux funs set
  where
    aux fs xs = do
      x <- xs
      powerF fs x

-- max R . (| thin Q . Λ ( S . F ∈ ) |)
-- = max R . (| thin Q . E S . ΛF ∈ |)
solverThinning :: Eq b => Funs a b -> Order b -> Order b -> List a -> b
solverThinning funs r q = maxSet r . foldF (thinSet q . mapE funs . cppL)


-- (| max R . Λ S  |)
solverGreedy :: Eq b => Funs a b -> Order b -> List a -> b
solverGreedy funs q = foldF ( maxSet q . powerF funs )


-- max R . (| Λ ( S . F ∈ ) |)
solverNaive :: Eq b => Funs a b -> Order b -> List a -> b
solverNaive funs r = maxSet r . foldF ( mapE funs . cppL)


-- max R . filter p . (| Λ ( S . F ∈ ) |)
-- = max R . filter p . (| E S . ΛF ∈ ) |)
solverFilterNaive :: Eq b => Funs a b -> Predicate b -> Order b -> List a -> b
solverFilterNaive funs p r = maxSet r . filter p . generator
  where
    generator = foldF ( mapE funs . cppL )

data Mode = Thinning | Greedy | FilterNaive | Naive

solverMain :: Eq b => Funs a b -> Predicate b -> Order b -> Order b -> Mode -> List a -> b
solverMain funs p r q mode =
  case mode of
    Thinning -> solverThinning funs r q
    Greedy   -> solverGreedy funs q
    Naive    -> solverNaive funs r
    FilterNaive    -> solverFilterNaive funs p r
-------------------------------------------------

out (In x) = x
nil x = In (Inl One)
cons (Inr x) = In (Inr x)
outl (Inr (Cross x y)) = x
outr (Inr (Cross x y)) = y
headL = outl.out

sumL :: List Int -> Int
sumL = foldF plusF
  where plusF (Inl One) = 0
        plusF (Inr (Cross a b)) = a + b


lengthL :: List a -> Int
lengthL = foldF plusF
  where plusF (Inl One) = 0
        plusF (Inr (Cross a b)) = 1 + b

-------------------------------------------------

subsequences :: Eq a => List a -> Set (List a)
subsequences = foldF ( mapE funs . cppL )
  where
    funs = (funs1,funs2)
    funs1 = [ Just . nil ]
    funs2 = [ Just . cons , Just . outr ]

inits :: Eq a => List a -> Set (List a)
inits = foldF ( mapE funs . cppL )
  where
    funs = (funs1,funs2)
    funs1 = [ Just . nil ]
    funs2 = [ Just . cons , Just . nil ]

tails' :: List a -> Set (List a)
tails' = foldF tailF
  where tailF (Inl One) = wrap $ In (Inl One)
        tailF (Inr (Cross a (x:xs))) = (In (Inr (Cross a x))) : x : xs

segments :: Eq a => List a -> Set (List a)
segments (In (Inl One)) = wrap $ In (Inl One)
segments (a@(In (Inr (Cross x xs)))) = inits a `union` (segments xs)

-------------------------------------------------

merge :: (a -> a -> Bool) -> (Set a,Set a) -> Set a
merge r ([],ys) = ys
merge r (xs,[]) = xs
merge r (a:xs,b:ys) | a `r` b = a : merge r (xs,b:ys)
                    | otherwise = b : merge r (a:xs,ys)

-------------------------------------------------


