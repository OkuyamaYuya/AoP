(define-const xs (List Int) 
  (insert 1 (insert 0 nil))
)

(declare-const ys (List Int))

(assert (not (= xs nil)))
(assert (not (= ys nil)))
(assert (= xs ys))

(check-sat)
(get-model)
