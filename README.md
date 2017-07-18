# GD

This DSL checks whether Greedy algorithm or Dynamic programming can be used to solve an optimization problem, derive a efficient program.

### Example

```haskell
-- 0-1 knapsack problem

ITYPE : (Int,Int)
OTYPE : List (Int,Int)
BASE : [nil]
STEP : [cons,outr]
INSTANCE : [(50,4),(3,12),(1,1),(10,5),(4,31),(4,2)]


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

```

This is an input program representing 0-1 knapsack problem. GD system takes this as input, and generates haskell program which solves this 0-1 knapsack problem.
