import ListCata
import Data.List (union)
leq = (<=)


lfuns = [ Just . e1 ]
rfuns = [ Just . f1 , Just . f2 ]
input_data = [(50 , 4),(3 , 12),(1 , 1),(10 , 5),(4 , 31),(4 , 2)]

pairPlus p1 p2 = (((fst p1) + (fst p2)) , ((snd p1) + (snd p2)))

pairSum  = foldF s
  where
    s (Inl One) = (0 , 0)
    s (Inr (Cross a b)) = pairPlus a b

sumVal x = (fst (pairSum x))

sumWt x = (snd (pairSum x))

w = 10

p x = ((leq (sumWt x)) w)

r a b = ((leq (sumVal a)) (sumVal b))

q a b = (((leq (sumVal a)) (sumVal b)) && ((sumWt a) == (sumWt b)))

e1 = nil

f1 = cons

f2 = outr
thin_or_greedy = solverMain (lfuns,rfuns) p r q
main =  print.fromList.thin_or_greedy Thinning $ toList input_data