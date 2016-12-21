-- 0-1 knapsack problem

TYPE : (Int,Int)
BASE : [e1]
STEP : [f1,f2]
INSTANCE : [ (50,4),(3,12),(1,1),(10,5),(4,31),(4,2)]


pairPlus p1 p2 :
  (Int,Int)->(Int,Int)->(Int,Int) =
  (fst p1 + fst p2 , snd p1 + snd p2)

pairSum : (List (Int,Int))->(Int,Int) = foldr pairPlus (0,0)

sumVal x : (List (Int,Int))->Int = fst (pairSum x)

sumWt x : (List (Int,Int))->Int = snd (pairSum x)

w : Int = 10

p x : (List (Int,Int))->Bool = (sumWt x) <= w


r a b :
  (List (Int,Int))->(List (Int,Int))->Bool = sumVal a <= sumVal b

q a b :
  (List (Int,Int))->(List (Int,Int))->Bool = (sumVal a <= sumVal b) && (sumWt a == sumWt b)

e1 : (List (Int,Int)) = nil
f1 : ((Int,Int),(List (Int,Int))) -> (List (Int,Int)) = cons
f2 : ((Int,Int),(List (Int,Int))) -> (List (Int,Int)) = outr
