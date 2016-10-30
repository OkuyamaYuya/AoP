
; tuple
; example => (declare-const p1 (Pair Int Int))
(declare-datatypes (T1 T2) ((Pair (mk-pair (first T1) (second T2)))))

; pair-plus
; example => (1,2) + (4,2) = (5,4)
(define-fun pair-plus ((x (Pair Int Int)) (y (Pair Int Int))) (Pair Int Int)
  (mk-pair (+ (first x) (first y)) (+ (second x) (second y))))

; pair-sum :: [(a,b)] -> (a,b)
(declare-fun pair-sum ((List (Pair Int Int))) (Pair Int Int))
(assert (forall ((xs (List (Pair Int Int))))
          (ite (= nil xs)
               (= (mk-pair 0 0)                              (pair-sum xs))
               (= (pair-plus (head xs) (pair-sum (tail xs))) (pair-sum xs)))))

; (a,x) `r` (b,y) = sum a >= sum b && sum x == sum y
(declare-fun r ((List (Pair Int Int)) (List (Pair Int Int))) Bool)
(assert (forall ((a (List (Pair Int Int))) (b (List (Pair Int Int))))
  (ite  (and (>= (first (pair-sum a)) (first (pair-sum b)))  (= (second (pair-sum a)) (second (pair-sum b))))
        (= true (r a b))
        (= false (r a b)))))

; not forall xs,ys,a . xs `r` ys => (a:xs) `r` (a:ys)
(assert (not (forall ((xs (List (Pair Int Int))) (ys (List (Pair Int Int))) (a (Pair Int Int)))
          (implies (r xs ys) (r (insert a xs) (insert a ys))))))
(check-sat)

