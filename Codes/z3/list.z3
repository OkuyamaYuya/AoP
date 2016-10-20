
;forall a:Int, xs,ys:List Int . (xs = ys) => (a:xs) = (a:ys)
(assert
  (forall ( (x (List Int)) (y (List Int)) (a Int) ) 
      (implies (= x y) (= (insert a x) (insert a y)))))
(check-sat)

;not forall a:Int,xs,ys:List Int . (head xs) = (head ys) => (a:xs) = (a:ys)
(assert
  (not (forall ( (xs (List Int)) (ys (List Int)) (a Int) ) 
    (implies (= (head xs) (head ys)) (= (insert a xs) (insert a ys))))))
(check-sat)


