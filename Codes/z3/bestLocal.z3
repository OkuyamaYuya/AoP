
; sum
(declare-fun sum ((List Int)) Int)
(assert (forall ((xs (List Int)))
  (ite (= nil xs)
       (= 0                                (sum xs))
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
  (and (r x1 x2) 
       (r x2 x1)))


; min2 x y = if x `r` y then x else y
(define-fun min2 ((x1 (List Int)) (x2 (List Int))) (List Int)
   (ite (r x1 x2) x1 x2))


; ListSet (T)
(declare-datatypes (T) ((ListSet nilSet (cons (hd (List T)) (tl ListSet)))))

; mapInsert :: a -> ListSet a -> ListSet a
(declare-fun mapInsert ((Int) (ListSet Int)) (ListSet Int))
(assert (forall ((a Int) (xs (ListSet Int)))
          (ite (= nilSet xs)
               (= (cons (insert a (hd xs)) nilSet) (mapInsert a xs))
               (= (cons (insert a (hd xs)) (mapInsert a (tl xs))) (mapInsert a xs)))))

; minR :: ListSet a -> List a
(declare-fun minR ((ListSet Int)) (List Int))
(assert (forall ((xs (ListSet Int)))
  (ite (= xs (cons nil nilSet))
       (= (hd xs) (minR xs))
       (= (min2 (hd xs) (minR (tl xs))) (minR xs)))))


; Best-Local
; sum (a:(minR xs)) = sum . minR $ (map (a:) xs)
;(assert (not (forall ((a Int) (xs (ListSet Int)))
;  (eqR (insert a (minR xs)) (minR (mapInsert a xs))))))

(check-sat)

