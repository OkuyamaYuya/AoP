-- 0-1 knapsack problem

pairPlus p1 p2 : (Pair Int Int)->(Pair Int Int)->(Pair Int Int) = (fst p1 + fst p2 , snd p1 + snd p2)

pairSum : (List (Pair Int Int))->(Pair Int Int) = foldr pairPlus (0,0)

sumVal x : (List (Pair Int Int))->Int = fst (pairSum x)

sumWt x : (List (Pair Int Int))->Int = snd (pairSum x)

w : Int = 10

within x : (List (Pair Int Int))->Bool = leq (sumWt x) w

knapR a b : (List (Pair Int Int))->(List (Pair Int Int))->Bool =
                                        leq (sumVal a) (sumVal b)

knapQ a b : (List (Pair Int Int))->(List (Pair Int Int))->Bool =
                (leq (sumVal a) (sumVal b))&&((sumWt a)==(sumWt b))


-- e1 : (List (Pair Int Int)) = nil
-- f1 : Int -> (List Int) -> (List Int) = cons
-- f2 : Int -> (List Int) -> (List Int) = outr
