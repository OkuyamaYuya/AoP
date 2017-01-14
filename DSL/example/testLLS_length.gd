-- lexicographically largest subsequences

ITYPE : Int
OTYPE : List Int
BASE  : [nil]
STEP : [cons,outr]
INSTANCE : [5,3,-3,3,34,5,7,2,3,55,65,4,-1,1,2]

plus1 a as : Int -> (List Int) -> Int = 1
length : (List Int)->Int = foldr plus1 0

r : (List Int)->(List Int)->Bool = leq_lexico
q : (List Int)->(List Int)->Bool = leq_lexico

p x : (List Int) -> Bool = length x <= 3

