; An interpreter for a Java-like language. Uses simpleParser.scm to parse the input.
; EECS 345 Programming Language Concepts Part 2
; Aditya Malik, Ayush Karnawat

; -----------------------------------------Environment/State----------------------------------------------

; The Initial State made with initial clean parameters
(define newenv
  (lambda ()
    '(((true false) (#t #f)))))

; Modifies the state by adding a layer to it
(define push_layer
  (lambda (env)
    (cons '(() ()) env)))

; Modifies the state by removing a layer from it
(define pop_layer
  (lambda (env)
    (cdr env)))

; Modifies the current state by adding a variable and value to the layer. 
(define addtoEnv
  (lambda (var value env)
    (if (eq? 'undeclared value)
        (error "ERROR: Undeclared variable" var)
        (call/cc
         (lambda (escape)
           (addtoEnv-help var value env (lambda () (escape (cons (add_to_layer var value (first env)) (rest env))))))))))

; Method used to check whether a variable is defined before in the state
(define lookup_list
  (lambda (var env)
    (cond
      ((null? env) 'undeclared)
      ((null? (first env)) (lookup_list var (rest env)))
      ((not (eq? 'undeclared (lookup var (first env)))) (lookup var (first env)))
      (else (lookup_list var (rest env))) )))


; -----------------------------------------Helper Functions----------------------------------------------

; Helper method used by addtoEnv. It helps modify the state by adding a variable name and value to each layer.
(define add_to_layer
  (lambda (var value env)
    (cond
      ((null? (first env))
       (cons (append (first env) (cons var '())) (cons (append (second env) (cons value '())) '())))
      ((eq? (car (first env)) var)
       (cons (first env)  (cons (cons value (cdr (second env))) '())) )
      (else (cons (cons (car (first env)) (car (add_to_layer var value (cons (cdr (first env))
                                                                                        (cons (cdr (second env)) '()))) )) (cons (cons (car (second env)) (car (cdr (add_to_layer var value
                                                                                                                                                                                          (cons (cdr (first env)) (cons (cdr (second env)) '()))) ))) '()) )))))

; Helper method used by addtoEnv
(define addtoEnv-help
  (lambda (var value env escape)
    (cond
      ((null? env) (escape))
      ((not (eq? 'undeclared (lookup var (first env)))) (cons (add_to_layer var value (first env)) (rest env)))
      (else (cons (first env) (addtoEnv-help var value (rest env) escape))))))


; Helper method to lookup the variable in the current state 
(define lookup
  (lambda (varName env)
    (cond
      ((or (null? env) (null? (first env))) 'undeclared)
      ((eq? varName (first (first env))) (first (second env)))
      (else (lookup varName (cons (rest (first env)) (cons (rest (second env)) '())))) )))