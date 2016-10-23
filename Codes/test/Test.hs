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
        f (Cross a (accumVal,accumWt)) = ( value a + accumVal , weight a + accumWt )

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
knapF = constF (funs1,funs2)
  where
    p = within 10
    funs1 = [ Just . nil ]
    funs2 = [ Just . outr , test p . cons ]
-- knapF p One = wrap $ nil One
-- knapF p x = (wrap $ outr x) `union` (test p $ cons x)

knapMain = solverMain knapF (within 10) knapR knapQ

-------------------------------------------------
-- Lexicographically Largest Subsequences
-------------------------------------------------

-- global criterion
llsR :: Order (T Char)
llsR = (<=)
-- local criterion
llsQ :: Order (T Char)
llsQ = (<=)

llsF :: Ord a => Step a (T a)
llsF = constF (funs1,funs2)
  where
    funs1 = [ Just . nil ]
    funs2 = [ Just . outr , Just . cons ]
-- llsF p One = wrap $ nil One
-- llsF p x = (wrap $ outr x) `union` (wrap $ cons x)

llsMain = solverMain llsF (\x->True) llsR llsQ


-------------------------------------------------
-- Driving problem
-------------------------------------------------
--
-- better-local Greedy
-- local optimality /= global optimality
-- 
-- Thinning problem when expressed by Catamorphism
--
-- min R . filter p . Λ (| S |)
-- can't write simple predicate
--

data Stop = Stop { pos :: Int } deriving (Show,Eq,Ord)

driveR :: Order (T Stop)
driveR a b = lengthT a >= lengthT b

driveQ :: Order (T Stop)
driveQ a b = lengthT a >= lengthT b && doko a <= doko b
  where
    doko :: T Stop -> Int
    doko (InT One) = 0
    doko (InT(Cross a b)) = pos a

gasOK :: Int -> Stop -> Predicate (T Stop)
gasOK full goal = \x -> ( fst (foldF f (cons $ Cross goal x)) <= full)
  where
    f :: F Stop (Int,Stop) -> (Int,Stop)
    f One = (0,Stop 0) -- (maximux distance,previous position)
    f (Cross cur (maxDist,preStop)) = (max maxDist curDist,cur)
      where curDist = pos cur - pos preStop

driveF :: Step Stop (T Stop)
driveF = constF (funs1,funs2)
  where
    funs1 = [ Just . nil ]
    funs2 = [ fun cons , fun outr ]
    fun g (x@(Cross a b)) = test (gasOK 70 a) (g x)

driveMain = solverMain driveF (gasOK 70 (Stop 300)) driveR driveQ

-------------------------------------------------
-- test case
-------------------------------------------------

knapFuns = map knapMain [ Naive,Thinning ]
itemss    = [ items1 , items2 ]
items1 = toT [ Item 50 4, Item 3 12, Item 1 1 ,  Item 10 5,
               Item 40 5, Item 30 6, Item 100 2, Item 3 4,
               Item 4 53, Item 4 2 , Item 32 3 , Item 3 2 ]
items2 = toT [ Item 10 5 , Item 40 5 , Item 30 5 , Item 50 5 , Item 100 5]

llsFuns = map llsMain [ Thinning,Greedy ]
strings  = [ string1 , string2 ]
string1 = toT "todai"
string2 = toT "universityoftokyo"

driveFuns = map driveMain [ FilterNaive,Naive,Greedy,Thinning ]
stopss = [ stops1 , stops2 ]
stops1 = toT $ map Stop [300,260,190,180,170,120,90,50,20,5]
stops2 = toT $ map Stop [300,260,190,170,120,90,40]


fff (funs,inputs) =
  mapM_ (print.fromT) $ do
    input <- inputs
    fun <- funs
    return $ fun input
-------------------------------------------------

main :: IO()
main = do
  -- fff (knapFuns,itemss)
  -- fff (llsFuns,strings)
  -- fff (driveFuns,stopss)
  (print.fromT) $ maxSet driveR . foldF (mapE driveF . cppF) $ stops2
  -- (print.fromT) $ foldF (maxSet driveR . driveF) $ stops2
  (print.fromT) $ maxSet driveQ . foldF (thinSet driveQ . mapE driveF . cppF) $ stops2
  -- mapM_ (print.fromT) $ filter (gasOK 70 (Stop 300)) $ subsequences stops2
  -- print.fromT $ maxSet driveR $ filter (gasOK 70 (Stop 300)) $ subsequences stops2
