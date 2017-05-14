#lang racket

(define len
  (lambda (l)
    (if (null? l)
        0
        (+ 1 (len (cdr l))))))

(define leng
  (lambda (l)
    (cond
      ((null? l) 0)
      (else
       (+ 1 (len (cdr l)))))))

(define member?
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((eq? a (car l)) #t)
      (else (member? a (cdr l))))))

; define factorial

; define countnum : counts number of times a num occurs 
(define countnum
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((eq? a (car l)) (+ 1 (countnum a (cdr l))))
      (else (countnum a (cdr l))))))

