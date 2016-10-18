
(assert
  (forall ( (x (List Int)) (y (List Int)) ) 
    (and (= (head x) (head y)) (= (insert 1 x) (insert 1 y)) )
  )
)

(check-sat)

(assert
  (forall ( (x (List Int)) (y (List Int)) ) 
    (implies (= x y) (= (insert 1 x) (insert 1 y)))
  )
)

(check-sat)


