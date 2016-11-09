(declare-fun leq_lexico ((List Int) (List Int)) Bool)
(assert (forall ((xs (List Int)) (ys (List Int)))
  (= (leq_lexico xs ys)
  (or
    (= xs nil)
    (< (head xs) (head ys)) 
    (and (= (head xs) (head ys)) 
         (leq_lexico (tail xs) (tail ys)))))))

(push)
(assert (leq_lexico (insert 100 nil) (insert 1 nil)))
(check-sat)
(pop)

(push)
(echo "Is connected ?")
(assert (not
    (forall ((x (List Int)) (y (List Int)))
          (or
            (leq_lexico x y)
            (leq_lexico y x)
    ))
))
(check-sat)
(pop)
