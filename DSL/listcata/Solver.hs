import ListCata
import Data.List (union)
leq = (<=)


input_data = [1,-1,1,-3,10,-1,2,-111,2,2]
lfuns = [ Just . const e1 ]
rfuns = [ Just . f1 , Just . f2 ]

plus x y = (x + y)

mySum  = foldF s
  where
    s (Inl One) = 0
    s (Inr (Cross a b)) = plus a b

r a b = ((leq (mySum a)) (mySum b))

q a b = ((leq (mySum a)) (mySum b))

p x = True

e1 = nil

f1 = cons

f2 a = nil
thin_or_greedy = solverMain (lfuns,rfuns) p r q
main =  print.fromList.thin_or_greedy Greedy $ toList input_data