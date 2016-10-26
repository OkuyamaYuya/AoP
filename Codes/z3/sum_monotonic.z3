
; sum
(declare-fun sum ((List Int)) Int)
(assert (forall ((xs (List Int)))
          (ite (= nil xs)
               (= 0                     (sum xs))
               (= (+ (head xs) (sum (tail xs))) (sum xs)))))

; a `r` b = sum a >= sum b
(declare-fun r ((List Int) (List Int)) Bool)
(assert (forall ((a (List Int)) (b (List Int)))
  (ite (>= (sum a) (sum b))
        (= true (r a b))
        (= false (r a b)))))

; not forall xs,ys,a . xs `r` ys => (a:xs) `r` (a:ys)
(assert (not (forall ((xs (List Int)) (ys (List Int)) (a Int))
          (implies (r xs ys) (r (insert a xs) (insert a ys))))))
(check-sat)

;(assert (not (forall ((xs (List Int)) (ys (List Int)) (a Int))
;          (implies (>= (sum xs) (sum ys)) (>= (sum (insert a xs)) (sum (insert a ys)))))))
;(check-sat)

