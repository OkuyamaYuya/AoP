{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

import ListCata
import Data.List (union)
import Debug.Trace


-------------------------------------------------
-- Examples
-------------------------------------------------
--
-- 0-1 knapsack problem
-- Lexicographically largest subseqences
-- Midas driving problem
--
--



-------------------------------------------------
-- 0-1 knapsack problem
-------------------------------------------------

data Item = Item { value :: Int , weight :: Int  } deriving (Show,Eq)

sumBoth :: List Item -> (Int,Int)
sumBoth = foldF f
  where f (Inl Nil) = (0,0)
        f (Inr (Cons a (accumVal,accumWt))) = ( value a + accumVal , weight a + accumWt )

sumVal :: List Item -> Int
sumVal = fst . sumBoth

sumWt :: List Item -> Int
sumWt = snd . sumBoth

-- predicate
within :: Int -> Predicate (List Item)
within w = \x -> (sumWt x <= w)

-- global criterion
knapR :: List Item -> List Item -> Bool
knapR a b = sumVal a <= sumVal b

-- local criterion
knapQ :: List Item -> List Item -> Bool
knapQ a b = let (va,wa) = sumBoth a
                (vb,wb) = sumBoth b in
                wa == wb && va <= vb


knapF :: Funs Item (List Item)
knapF = (funs1,funs2)
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
--
-- local criterion = global criterion
--

-- global criterion
llsR :: Order (List Char)
llsR = (<=)
-- local criterion
llsQ :: Order (List Char)
llsQ = (<=)

llsF :: Ord a => Funs a (List a)
llsF = (funs1,funs2)
  where
    funs1 = [ Just . nil ]
    funs2 = [ Just . outr , Just . cons ]
-- llsF p One = wrap $ nil One
-- llsF p x = (wrap $ outr x) `union` (wrap $ cons x)

llsMain = solverMain llsF (\x->True) llsR llsQ


-------------------------------------------------
-- Midas Driving problem
-------------------------------------------------
--
-- better-local Greedy
-- local optimality /= global optimality
--
-- min R . filter p . Λ (| S |)
-- not simple predicate
--
-- 次のガソリンスタンドまでの距離を Stop に持たせれば、
-- Greedy で解ける. この場合、局所順序は遠くまでいっているか
-- になって、大域順序と異なる.
--
-- 以下は、thinning で解いている.
--



data Stop = Stop { pos :: Int } deriving (Show,Eq,Ord)

driveR :: Order (List Stop)
driveR a b = lengthL a >= lengthL b

driveQ :: Order (List Stop)
driveQ a b = lengthL a >= lengthL b && doko a <= doko b
  where
    doko :: List Stop -> Int
    doko (In (Inl Nil)) = 0
    doko (In (Inr (Cons a b))) = pos a

gasOK :: Int -> Stop -> Predicate (List Stop)
gasOK full goal = \x -> (fst (foldF f (cons $ Inr (Cons goal x))) <= full)
  where
    f (Inl Nil) = (0,Stop 0) -- (maximux distance,previous position)
    f (Inr (Cons cur (maxDist,preStop))) = (max maxDist curDist,cur)
      where curDist = pos cur - pos preStop

driveF :: Funs Stop (List Stop)
driveF = (funs1,funs2)
  where
    funs1 = [ Just . nil ]
    funs2 = [ aux cons , aux outr ]
    aux g (x@(Inr (Cons a b))) = test (gasOK 70 a) (g x)

driveMain mode x = solverMain driveF (gasOK 70 (headL x)) driveR driveQ mode x

-------------------------------------------------
-- test case
-------------------------------------------------

knapFuns = map knapMain [ Naive,Thinning ]
itemss    = [ items1 , items2 ]
items1 = toList [ Item 50 4, Item 3 12, Item 1 1 ,  Item 10 5,
               Item 40 5, Item 30 6, Item 100 2, Item 3 4,
               Item 4 53, Item 4 2 , Item 32 3 , Item 3 2 ]
items2 = toList [ Item 10 5 , Item 40 5 , Item 30 5 , Item 50 5 , Item 100 5]

llsFuns = map llsMain [ Thinning,Greedy ]
strings  = [ string1 , string2 ]
string1 = toList "todai"
string2 = toList "universityoftokyo"

driveFuns = map driveMain [ FilterNaive,Naive,Thinning,Greedy ]
stopss = [ stops1 , stops2 ]
stops1 = toList $ map Stop [300,260,190,180,170,120,90,50,20,5]
stops2 = toList $ map Stop [300,260,190,170,120,90,40]


fff (funs,inputs) =
  mapM_ (print.fromList) $ do
    input <- inputs
    fun <- funs
    return $ fun input

-------------------------------------------------
-- debug
-------------------------------------------------

printArg f x = f (trace (show x) x)

debugGreedy funs q = foldF ( printArg (maxSet q . powerF funs) )

debugThinning funs r q = maxSet r . foldF ( hhh (thinSet q) . mapE funs . cppL )
  where hhh f x = trace ("--\n"++(show (trans x)++"\n"++(show $ trans (f x)))) (f x)
        trans = mapSet (map pos.fromList)

-- scanr Thinning
thinSet' q = scanr step []
  where step a []     = [a]
        step a (b:xs) | a `q` b = b : xs
                      | b `q` a = a : xs
                      | otherwise = b : step a xs

-------------------------------------------------


main :: IO()
main = do
  fff (knapFuns,itemss)
  fff (llsFuns,strings)
  fff (driveFuns,stopss)


