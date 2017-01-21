-- lexicographically largest subsequences

ITYPE : Int
OTYPE : List Int
BASE  : [nil]
STEP : [cons,outr]
INSTANCE : [5,3,-3,3,74,5,7,2,3,55,4,-1,1,2]

r : (List Int)->(List Int)->Bool = leq_lexico
q : (List Int)->(List Int)->Bool = leq_lexico

p x : (List Int) -> Bool = True

e1 : (List Int) = nil
f1 : Int -> (List Int) -> List Int = cons
f2 : Int -> (List Int) -> List Int = outr

