-- lexicographically largest subsequences

ITYPE : Int
OTYPE : List Int
BASE  : [nil]
STEP : [cons,outr]
INSTANCE : [5,3,-3,3,74,5,7,2,3,55,4,-1,1,2]

plus1 x y : Int -> Int -> Int = 1 + y
len : (List Int)->Int = foldr plus1 0

r : (List Int)->(List Int)->Bool = leq_lexico
q a b : (List Int)->(List Int)->Bool = leq_lexico a b && len a == len b

p x : (List Int) -> Bool = len x <= 3

