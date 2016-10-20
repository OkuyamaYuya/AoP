module DataStructure where
import Data.List (union)
-- base bi-functor : F
-- F a b = 1 + a x b
-- fix (F a) = T a
data F a b = Nil | Cons a b deriving (Show,Eq,Ord)
newtype T a = InT (F a (T a)) deriving (Show,Eq,Ord)

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
cppF Nil = wrap $ Nil
cppF (Cons a xss) = [ (Cons a xs) | xs <- xss ]

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
subsequences :: Eq a => T a -> Set (T a)
subsequences = foldF sbsqF
  where sbsqF Nil = wrap $ InT Nil
        sbsqF (Cons a xs) = [ InT (Cons a x) | x <- xs ] `union` xs
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

type Order a = a -> a -> Bool
type Step a b = Predicate b -> F a b -> Set b

solver_thinning :: Step a b -> Predicate b -> Order b -> Order b -> T a -> b
solver_thinning gF p r q = maxSet r . foldF (thinSet q . concat . (mapSet sF) . cppF)
  where sF = gF p

solver_greedy gF p r = foldF ( maxSet r . sF )
  where sF = gF p

solver_naive :: Step a b -> Predicate b -> Order b -> T a -> b
solver_naive gF p r = maxSet r . filter p . generator
  where 
    sF = gF (\x->True)
    generator = foldF ( concat . (mapSet sF) . cppF  )

