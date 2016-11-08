(declare-fun leq_lexico ((List Int) (List Int)) Bool)
(assert (forall ((xs (List Int)) (ys (List Int)))
  (= (leq_lexico xs ys)
  (or
    (= xs nil)
    (< (head xs) (head ys)) 
    (and (= (head xs) (head ys)) 
         (leq_lexico (tail xs) (tail ys)))))))

(assert (leq_lexico (insert 100 nil) (insert 1 nil)))

(check-sat)
