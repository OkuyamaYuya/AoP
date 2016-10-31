
; sum
(declare-fun sum ((List Int)) Int)
(assert (forall ((xs (List Int)))
  (ite (= nil xs)
       (= 0                             (sum xs))
       (= (+ (head xs) (sum (tail xs))) (sum xs)))))

; a `r` b = sum a <= sum b
; r :: list int -> list int -> bool
(declare-fun r ((List Int) (List Int)) Bool)
(assert (forall ((a (List Int)) (b (List Int)))
  (ite (<= (sum a) (sum b))
       (= true (r a b))
       (= false (r a b)))))


;a `eqR` b = a `r` b && b `r` a
(define-fun eqR ((x1 (List Int)) (x2 (List Int))) Bool
  (= (sum x1) (sum x2)))


; min2 x y = if x `r` y then x else y
(define-fun min2 ((x1 (List Int)) (x2 (List Int))) (List Int)
   (ite (r x1 x2) x1 x2))


; ListSet (T)
(declare-datatypes (T) ((ListSet nilSet (cons (hd (List T)) (tl ListSet)))))

; mapInsert :: a -> ListSet a -> ListSet a
(declare-fun mapInsert ((Int) (ListSet Int)) (ListSet Int))
(assert (forall ((a Int) (xs (ListSet Int)))
          (ite (= nilSet xs)
               (= nilSet (mapInsert a xs))
               (= (cons (insert a (hd xs)) (mapInsert a (tl xs))) (mapInsert a xs)))))

; minR :: ListSet a -> List a
(declare-fun minR ((ListSet Int)) (List Int))
(assert (forall ((xs (ListSet Int)) (x (List Int)))
  (ite (= xs (cons x nilSet))
       (= x  (minR xs))
       (= (min2 (hd xs) (minR (tl xs))) (minR xs)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare-const xss (ListSet Int))
(assert (= xss (cons (insert 4 nil) (cons (insert 1 (insert 3 nil)) nilSet))))
(assert (= false (forall ((a Int)) (eqR (insert a (minR xss)) (minR (mapInsert a xss)))))) ; => , takes 
;(assert (= false 
;        (forall ((yss (ListSet Int)))
;          (=> (not (= yss nilSet))
;              (eqR (insert 1 (minR yss)) (minR (mapInsert 1 yss))))))) ; => , takes 
;(assert (= false (eqR (insert 2 (minR xss)) (minR (mapInsert 2 xss))))) ; => unsta , takes 5 seconds
;(assert (= false (= (sum (minR (mapInsert 0 xss))) (sum (minR xss))))) ; => unsat , takes 10 seconds
;(assert (= true (= 5 (sum (minR (mapInsert 2 xss)))))) ; => unsat , takes 10 seconds
;(assert (= false (= 6 (sum (minR (mapInsert 2 xss)))))) ; => unsat , takes 2 minutes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Best-Local
; sum (a:(minR xs)) = sum . minR $ (map (a:) xs)
;(assert (= false 
;         (forall ((a Int) (xs (ListSet Int))) 
;            (=> (not (= xs nilSet))
;                (eqR (insert a (minR xs)) (minR (mapInsert a xs))))))

(check-sat)


