-- 0-1 knapsack problem

BASETYPE : Pair Int Int
LEFT     : [e1]
RIGHT    : [f1,f2]
INPUT : [ (50,4),(3,12),(1,1),(10,5),(4,31),(4,2)]


pairPlus p1 p2 :
  (Pair Int Int)->(Pair Int Int)->(Pair Int Int) =
  (fst p1 + fst p2 , snd p1 + snd p2)

pairSum : (List (Pair Int Int))->(Pair Int Int) = foldr pairPlus (0,0)

sumVal x : (List (Pair Int Int))->Int = fst (pairSum x)

sumWt x : (List (Pair Int Int))->Int = snd (pairSum x)

w : Int = 10

p x : (List (Pair Int Int))->Bool = leq (sumWt x) w

r a b :
  (List (Pair Int Int))->(List (Pair Int Int))->Bool =
  leq (sumVal a) (sumVal b)

q a b :
  (List (Pair Int Int))->(List (Pair Int Int))->Bool =
  (leq (sumVal a) (sumVal b)) && ((sumWt a) == (sumWt b))

e1 : (List (Pair Int Int)) = nil
f1 : (Pair Int Int) -> (List (Pair Int Int)) -> (List (Pair Int Int)) = cons
f2 : (Pair Int Int) -> (List (Pair Int Int)) -> (List (Pair Int Int)) = outr
