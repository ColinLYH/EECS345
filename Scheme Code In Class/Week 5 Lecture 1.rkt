#lang racket

; define factorial, the conventional way
(define fact
  (lambda (n)
    (if (zero? n)
        1
        (* n (fact (- n 1))))))
(fact 4)
= (* 4 (fact 3))
= (* 4 (* 3 (fact 2)))
= (* 4 (* 3 (* 2 (fact 1))))
= (* 4 (* 3 (* 2 (* 1 (fact 0)))))
= (* 4 (* 3 (* 2 (* 1 1))))
= (* 4 (* 3 (* 2 1)))
= (* 4 (* 3 2))
= (* 4 6)
= 24

(define fact-iter
  (lambda (n)
    (fact-iter-acc n 1)))
(define fact-iter-acc
  (lambda (n acc)
    (if (zero? n)
        acc
        (fact-iter-acc (- n 1) (* n acc)))))


(fact-iter 4)
= (fact-iter-acc 4 1)
= (fact-iter-acc (- 4 1) (* 4 1))
= (fact iter-acc 3 4 )
= (fact-iter-acc (- 3 1) (* 3 4))
= (fact-iter-acc 2 12)
= (fact-iter-acc (- 2 1) (*2 12))
= (fact-iter-acc 1 24)
= (fact-iter-acc (- 1 1) (*1 24))
= 24

; continuation passing style
(* 4 (* 3 (* 2 (fact 1))))
(* 4 (* 3 (* 2 ________)))

(lambda (v) (* 4 (* 3 (* 2 v))))
; k is (lambda (v) v)
(define fact-cps
  (lambda (n k)
    (if (zero? n)
        (k 1)
        (fact-cps (- n 1)
                  (lambda (v) (k (* 4 v)))))))
(fact-cps 4 k)
(fact-cps 3 (lambda (v) (k (* 4 v))) ) ; n = 4, (- n 1) = 3
(fact-cps 2 (lambda (v)
              ( (lambda (v) (k (* 4 v))) (* 4 v))
              (* 3 v) ))
(fact-cps 2 (lambda (v) (k (* 4 (* 3 v)))) ) ; this will compute the same thing
(fact-cps 1 (lambda (v)
              (lambda (v) (k (* 4 (* 3 v))))
              (* 2 v) ))
(fact-cps 1 (lambda (v) (k (* 4 (* 3 (* 2 v))))))
(fact-cps 0 (lambda (v)
              (k (* 4 (* 3 (* 2 v))))
              (* 1 v)))

(define k (lambda(v) v)) (k 1) -> 1
(fact cps (* 4 (* 3 (* 2 (* 1 v))))
      1)
(k (* 4 (* 3 (* 2 (* 1 v)))))
(k 24)

(fact-cps 0 (lambda (v) (k (* 4 (* 3 (* 2 (* 1 v))))) ))







