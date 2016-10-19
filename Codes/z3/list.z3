
;forall a:Int, xs,ys:List Int . (xs = ys) => (a:xs) = (a:ys)
(assert
  (forall ( (x (List Int)) (y (List Int)) (a Int) ) 
      (implies (= x y) (= (insert a x) (insert a y)))))
(check-sat)

;not forall xs,ys:List Int . (head xs) = (head ys) => (1:xs) = (1:ys)
(assert
  (not (forall ( (xs (List Int)) (ys (List Int)) ) 
    (implies (= (head xs) (head ys)) (= (insert 1 xs) (insert 1 ys))))))
(check-sat)

