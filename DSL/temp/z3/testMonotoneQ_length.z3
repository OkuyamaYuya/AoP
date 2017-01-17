(declare-datatypes (T1 T2) ((Pair (mk-pair (fst T1) (snd T2)))))
(define-fun cons ((x  Int ) (y (List Int ))) (List  Int )
  (insert x y))
(define-fun outr ((x  Int ) (y (List Int ))) (List  Int )
  y)
(define-fun leq ((x Int) (y Int)) Bool
(<= x y))
(declare-fun leq_lexico ((List Int) (List Int)) Bool)
(assert (forall ((xs (List Int)) (ys (List Int)))
  (= (leq_lexico xs ys)
  (or
    (= xs nil)
    (< (head xs) (head ys))
    (and (= (head xs) (head ys))
         (leq_lexico (tail xs) (tail ys)))))))


(declare-fun length ((List Int)) Int)
(assert (forall ((xs (List Int)))
  (ite (= nil xs)
       (= 0    (length xs))
       (= (+ 1 (length (tail xs))) (length xs)))))



(define-fun r ((x1  (List  Int ))(x2  (List  Int ))) Bool 
(leq_lexico x1 x2 ))

(define-fun q ((x1  (List  Int ))(x2  (List  Int ))) Bool 
  (and (leq_lexico x1 x2) (= (length x1) (length x2))))

(define-fun p ((x (List  Int ))) Bool 
  (>= 3 (length x))
)


(push)
(declare-const bcons Bool)
(declare-const boutr Bool)
(assert (= bcons
    (forall ((xs (List  Int )) (ys (List  Int )) (a  Int ))
    (=> (q ys xs) 
    (=> (p (cons a ys))
        (or 
	(and (p (cons a xs)) (q (cons a ys) (cons a xs)))
	(and (p (outr a xs)) (q (cons a ys) (outr a xs)))
))))))
(assert (= boutr
    (forall ((xs (List  Int )) (ys (List  Int )) (a  Int ))
    (=> (q ys xs) 
    (=> (p (outr a ys))
        (or 
	(and (p (cons a xs)) (q (outr a ys) (cons a xs)))
	(and (p (outr a xs)) (q (outr a ys) (outr a xs)))
))))))
(assert (not (and bcons boutr )))
(check-sat)
(pop)
