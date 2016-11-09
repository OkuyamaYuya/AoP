-- lexicographically largest subsequences

BASETYPE : Int
INPUT : [5,3,2,1,3,34,5,7,2,3,55,65,4,1,2]
LEFT  : [e1]
RIGHT : [f1,f2]

r : (List Int)->(List Int)->Bool = leq_lexico
q : (List Int)->(List Int)->Bool = leq_lexico

p x : (List Int) -> Bool = True

e1 : (List Int) = nil
f1 : Int -> (List Int) -> (List Int) = cons
f2 : Int -> (List Int) -> (List Int) = outr

