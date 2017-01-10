(declare-datatypes (T1 T2) ((Pair (mk-pair (fst T1) (snd T2)))))
(define-fun cons ((x (Pair  Int   Int )) (y (List(Pair  Int   Int )))) (List (Pair  Int   Int ))
  (insert x y))
(define-fun outr ((x (Pair  Int   Int )) (y (List(Pair  Int   Int )))) (List (Pair  Int   Int ))
  y)
(define-fun leq ((x Int) (y Int)) Bool
(<= x y))


(define-fun pairPlus ((p1 (Pair  Int   Int ))(p2 (Pair  Int   Int )))(Pair  Int   Int )
(mk-pair (+ (fst p1) (fst p2)) (+ (snd p1) (snd p2))) )

(define-fun p ((x (Pair  Int   Int ))) Bool 
(and (>= (fst x) 0) (>= (snd x) 0) (<= (snd x) 10) ))

(define-fun r ((a (Pair  Int   Int ))(b (Pair  Int   Int ))) Bool 
(<= (fst a) (fst b)) )

(define-fun q ((a (Pair  Int   Int ))(b (Pair  Int   Int ))) Bool 
(and (<= (fst a) (fst b)) (= (snd a) (snd b))) )

(define-fun f2 ((a (Pair  Int   Int ))(b (Pair  Int   Int )))(Pair  Int   Int )
b)

(declare-rel R ((Pair Int Int) (Pair Int Int) (Pair Int Int)))
(declare-var b (Pair Int Int))
(declare-var x (Pair Int Int))
(declare-var y (Pair Int Int))
(assert (forall ((b (Pair Int Int)) (x (Pair Int Int)) (y (Pair Int Int)))
      (=> (R y b x) 
          (or
            (= y (pairPlus b x))
            (= y (f2 b x))
          )
      )
))

(assert (not
    (forall ((xs (Pair Int Int)) (ys (Pair Int Int)) (a (Pair Int Int))  (axs (Pair Int Int)) (ays (Pair Int Int)))
    (=> (and (R ays a ys) (R axs a xs))
        (r ys xs)
        (r ays axs)))))
(check-sat)
(get-model)
