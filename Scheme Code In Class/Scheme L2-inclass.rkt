#lang racket

; L2-in class
; car = first element, cdr = sublist of all elements but first
; be able to write a function to sum all the numbers in a list (TEST QUESTION), so you test for
; each atom to see if its a number


; Today we will look at more recursive functions by using: cond, helper functions



; take a list and a number and return that element of the list
(define getIndex
  (lambda (l n) ; zero indexing
    (cond
      ((null? l) #f)
      ((zero? n) (car l))
      (else (getIndex (cdr l) (- n 1))))))

; makes a list of n elements containing element x
(define repeat
  (lambda (x n)
    (cond
      ((zero? n) '())
      (else (cons x (repeat x (- n 1)))))))

; myremove : removes the first occurence of the element a in a list l
(define myremove
  (lambda (a l)
    (cond
      ((null? l) '())
      ((eq? a (car l)) (cdr l))
      (else (cons (car l) (myremove a (cdr l))))))) 


; removeall ; removes all the occurences of the element a in a list l
(define removeall
  (lambda (a l)
    (cond
      ((null? l) '())
      ((eq? a (car l)) (removeall a (cdr l)))
      (else (cons (car l) (removeall a (cdr l)))))))


; myappend - appends two lists together
(define myappend
  (lambda (l1 l2)
    (cond
      ((null? l1) l2)
      (else (cons (car l1) (myappend (cdr l1) l2))))))

; myreserve - reverses the elements of a list -> DO FOR HOMEWORK, USE MYAPPEND
; (a b c) -> (c b a)
      