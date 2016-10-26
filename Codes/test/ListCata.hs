-------------------------------------------------
-- cons-List
-- Catamorphism
-------------------------------------------------
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators,FlexibleContexts,ConstrainedClassMethods #-}

module ListCata where

import Data.List (union,nub)
import Data.Maybe (maybeToList)

-------------------------------------------------

data M f a = In (f a (M f a))

data (f :+: g) a b = Inl (f a b) | Inr (g a b) deriving Eq

instance (Show a,Render f) => Show (M f a) where
  show = pretty

instance (Eq a,Eq2 f) => Eq (M f a) where
  (==) = (=:=)

instance (Functor (f a),Functor (g a)) => Functor ((f :+: g) a) where
  fmap f (Inl x) = Inl (fmap f x)
  fmap f (Inr x) = Inr (fmap f x)

instance (Show (f a b),Show (g a b)) => Show ((f :+: g) a b) where
  show (Inl x) = "Inl " ++ show x
  show (Inr x) = "Inr " ++ show x

class Render f where
  render :: (Show a,Render g) => (f a (M g a)) -> String

instance (Render f,Render g) => Render (f :+: g) where
  render (Inl x) = render x
  render (Inr x) = render x

pretty :: (Show a,Render f) => (M f a) -> String
pretty (In x) = render x

class Eq2 f where
  eq2 :: (Eq a,Eq2 g) => (f a (M g a)) -> (f a (M g a)) -> Bool

instance (Eq2 f,Eq2 g) => Eq2 (f :+: g) where
  (Inl x) `eq2` (Inl y) = x `eq2` y
  (Inr x) `eq2` (Inr y) = x `eq2` y

(=:=) :: (Eq a,Eq2 f) => (M f a) -> (M f a) -> Bool
(=:=) (In x) (In y) = x `eq2` y

-------------------------------------------------
-- cons List
-------------------------------------------------

data Nil a b  = Nil deriving (Show,Eq)
data Cons a b = Cons a b deriving (Show,Eq)

instance Functor (Nil a) where
  fmap f Nil = Nil
instance Functor (Cons a) where
  fmap f (Cons x y) = Cons x (f y)

instance Render Nil where
  render Nil = "[]"
instance Render Cons where
  render (Cons x xs) = show x ++ ":" ++ pretty xs

instance Eq2 Nil where
  Nil `eq2` Nil = True
instance Eq2 Cons where
  (Cons x xs) `eq2` (Cons y ys) = x == y && xs =:= ys

type L = Nil :+: Cons
type List a = M L a

-------------------------------------------------

foldF :: Functor (f a) => (f a b -> b) -> (M f a) -> b
foldF f (In x) = f ( fmap (foldF f) x )

toList :: [a] -> List a
toList [] = In (Inl Nil)
toList (x:xs) = In (Inr (Cons x (toList xs)))

fromList :: List a -> [a]
fromList (In (Inl Nil)) = []
fromList (In (Inr (Cons x xs))) = x : fromList xs

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
cppL (Inl Nil) = wrap $ Inl Nil
cppL (Inr (Cons x ys)) = [ Inr (Cons x y) | y <- ys ]

-------------------------------------------------

merge :: (a -> a -> Bool) -> (Set a,Set a) -> Set a
merge r ([],ys) = ys
merge r (xs,[]) = xs
merge r (a:xs,b:ys) | a `r` b = a : merge r (xs,b:ys)
                    | otherwise = b : merge r (a:xs,ys)

cpp :: (Set a,Set b) -> Set(a,b)
cpp (xs,ys) = [ (x,y) | x <- xs , y <- ys ]

cpr :: (a,Set b) -> Set(a,b)
cpr (x,ys) = [ (x,y) | y <- ys ]

split :: (a -> b) -> (a -> c) -> a -> (b,c)
split f g x = ( f x , g x )

-------------------------------------------------

-- input : S , R , Q
-- S = [ fs , gs ]
-- 
-- filter embedding naive
--   max R . Λ (| S |)
-- = max R . (| Λ ( S . F ∈ ) |)
-- = max R . (| E S . Λ F ∈   |)
-- 
-- filter naive
-- max R . filter p . (| E S . Λ F ∈ |)
--
-- thinning
-- max R . (| thin Q . E S . Λ F ∈  |)
-- 
-- greedy
-- (| max Q . Λ S |)
-- 
-- S :: F a b -> b
-- Λ S :: F a b -> Set b
-- (| S |) :: T a -> b
-- Λ ( S . F ∈ ) :: F a (Set b) -> Set b


-- preorder on type a
type Order a = a -> a -> Bool

type Funs a b = ( [L a b -> Maybe b] , [L a b -> Maybe b] )

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

-- type Funs a b = ( [L a b -> Maybe b] , [L a b -> Maybe b] )

out (In x) = x
nil _ = In (Inl Nil)
cons x = In x
outr (Inr (Cons a b)) = b

sumList :: List Int -> Int
sumList = foldF plusF
  where plusF (Inl Nil) = 0
        plusF (Inr (Cons a b)) = a + b

-- subsequences :: Eq a => List a -> Set (List a)
-- subsequences = foldF ( mapE funs . cppL )
--   where
--     funs = (funs1,funs2)
--     funs1 = [ Just . nil ]
--     funs2 = [ Just . cons , Just . outr ]

-- subsequences' :: Eq a => T a -> Set (T a)
-- subsequences' = foldF sbsqF
--   where
--     sbsqF One = wrap $ nil One
--     sbsqF (Cross a xss) = [ InT (Cross a xs) | xs <- xss ] `union` xss
--
-- inits :: Eq a => T a -> Set (T a)
-- inits = foldF ( mapE funs . cppF )
--   where
--     funs = (funs1,funs2)
--     funs1 = [ Just . nil ]
--     funs2 = [ Just . cons , Just . nil ]
--
-- inits' :: Eq a => T a -> Set (T a)
-- inits' = foldF initF
--   where
--    initF One = wrap $ nil One
--    initF (Cross a xss) = (wrap $ nil One) `union` (map (cons . (Cross a)) xss)
--
-- tails' :: T a -> Set (T a)
-- tails' = foldF tailF
--   where tailF One = wrap $ nil One
--         tailF (Cross a (x:xs)) = (InT (Cross a x)) : x : xs
--
-- segments :: Eq a => T a -> Set (T a)
-- segments (InT One) = wrap $ nil One
-- segments (a@(InT (Cross x xs))) = inits a `union` (segments xs)
-------------------------------------------------

main = do
  print $ toList [1..10]

