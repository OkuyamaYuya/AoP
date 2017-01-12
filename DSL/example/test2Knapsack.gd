-- 0-1 knapsack problem

ITYPE : (Int,Int)
OTYPE : (Int,Int)
BASE : [(0,0)]
STEP : [pairPlus,f2]
INSTANCE : [(50,4),(3,12),(1,1),(10,5),(4,31),(4,2)]


pairPlus p1 p2 :
  (Int,Int)->(Int,Int)->(Int,Int) =
  (fst p1 + fst p2 , snd p1 + snd p2)

p x : (Int,Int)->Bool = (snd x) <= 10

r a b :
  (Int,Int)->(Int,Int)->Bool = fst a <= fst b

q a b :
  (Int,Int)->(Int,Int)->Bool = (fst a <= fst b) && (snd a == snd b)

f2 a b : (Int,Int) -> (Int,Int) -> (Int,Int) = b
