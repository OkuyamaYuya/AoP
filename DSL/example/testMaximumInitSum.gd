-- maximum init sum

TYPE : Int
BASE : [e1]
STEP : [f1,f2]
INSTANCE : [1,-1,1,-3,10,-1,2,-111,2,2]

plus x y : Int -> Int -> Int = x + y
mySum : (List Int)->Int = foldr plus 0

r a b : (List Int)->(List Int)->Bool = mySum a <= mySum b
q a b : (List Int)->(List Int)->Bool = mySum a <= mySum b

p x : (List Int) -> Bool = True

e1 : List Int = nil
f1 : (Int , (List Int) ) -> (List Int) = cons
f2 a : (Int , (List Int) ) -> (List Int) = nil

