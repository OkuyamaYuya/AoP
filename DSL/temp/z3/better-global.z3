(declare-datatypes (T1 T2) ((Pair (mk-pair (fst T1) (snd T2)))))
(define-fun cons ((x (Pair  Int  (List  Int )))) (List  Int )
  (insert (fst x) (snd x)))
(define-fun outr ((x (Pair  Int  (List  Int )))) (List  Int )
  (snd x))
(define-fun leq ((x Int) (y Int)) Bool
(<= x y))

(define-fun plus ((x  Int )(y  Int )) Int 
(+ x y) )

(declare-fun mySum ((List  Int )) Int )
(assert (forall ((xs (List  Int )))
  (ite (= nil xs)
       (= 0    (mySum xs))
       (= (plus (head xs) (mySum(tail xs))) (mySum xs)))))

(define-fun r ((a (List  Int ))(b (List  Int ))) Bool 
(<= (mySum a) (mySum b)) )

(define-fun q ((a (List  Int ))(b (List  Int ))) Bool 
(<= (mySum a) (mySum b)) )

(define-fun p ((x (List  Int ))) Bool 
true )

(define-const e1 (List  Int )
nil)

(define-fun f1 ((x1  (Pair  Int  (List  Int ))))(List  Int )
(cons x1 ))

(define-fun f2 ((a (Pair  Int  (List  Int ))))(List  Int )
nil )

(push)
(declare-const bf1 Bool)
(declare-const bf2 Bool)
(assert (= bf1
    (forall ((xs (List  Int )) (ys (List  Int )) (a  Int ))
    (=> (q ys xs) 
    (=> (p (f1 (mk-pair a ys)))
        (or 
	(and (p (f1 (mk-pair a xs))) (q (f1 (mk-pair a ys)) (f1 (mk-pair a xs))))
	(and (p (f2 (mk-pair a xs))) (q (f1 (mk-pair a ys)) (f2 (mk-pair a xs))))
))))))
(assert (= bf2
    (forall ((xs (List  Int )) (ys (List  Int )) (a  Int ))
    (=> (q ys xs) 
    (=> (p (f2 (mk-pair a ys)))
        (or 
	(and (p (f1 (mk-pair a xs))) (q (f2 (mk-pair a ys)) (f1 (mk-pair a xs))))
	(and (p (f2 (mk-pair a xs))) (q (f2 (mk-pair a ys)) (f2 (mk-pair a xs))))
))))))
(assert (not (and bf1 bf2 )))
(check-sat)
(pop)
