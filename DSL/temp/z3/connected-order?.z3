; global order R
; local order  Q
; R is connected ?
; Q is connected ?

(declare-datatypes (T1 T2) ((Pair (mk-pair (first T1) (second T2)))))

(define-fun pair-plus ((x (Pair Int Int)) (y (Pair Int Int))) (Pair Int Int)
  (mk-pair (+ (first x) (first y)) (+ (second x) (second y))))

(declare-fun pair-sum ((List (Pair Int Int))) (Pair Int Int))
(assert (forall ((xs (List (Pair Int Int))))
          (ite (= nil xs)
               (= (mk-pair 0 0)                              (pair-sum xs))
               (= (pair-plus (head xs) (pair-sum (tail xs))) (pair-sum xs)))))

; a `r` b = sum a >= sum b
(declare-fun r ((List (Pair Int Int)) (List (Pair Int Int))) Bool)
(assert (forall ((a (List (Pair Int Int))) (b (List (Pair Int Int))))
  (ite (>= (first (pair-sum a)) (first (pair-sum b)))
        (= true (r a b))
        (= false (r a b)))))

; (a,x) `q` (b,y) = sum a <= sum b && sum x == sum y
(declare-fun q ((List (Pair Int Int)) (List (Pair Int Int))) Bool)
(assert (forall ((a (List (Pair Int Int))) (b (List (Pair Int Int))))
  (ite  (and (<= (first (pair-sum a)) (first (pair-sum b)))  (= (second (pair-sum a)) (second (pair-sum b))))
        (= true (q a b))
        (= false (q a b)))))


(push)
(echo "Is r connected ?")
(assert (= false
    (forall ((x (List (Pair Int Int))) (y (List (Pair Int Int))))
          (or
            (r x y)
            (r y x)
    ))
))
(check-sat)
(pop)


(push)
(echo "Is q connected ?")
(assert (= true
    (forall ((x (List (Pair Int Int))) (y (List (Pair Int Int))))
          (or
            (q x y)
            (q y x)
    ))
))
(check-sat)
(pop)
