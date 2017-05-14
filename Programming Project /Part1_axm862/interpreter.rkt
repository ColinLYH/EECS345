; Aditya Malik
; EECS 345
; Programming Project 1
; ----------------------------------------------------------------------------------------------------------------------
; The following are all interpreter functions

(load "simpleParser.scm")

; The following will prse the filename into a parsetree and pass it onto a runParsedList function that will read
; the list part by part 
(define interpret
  (lambda (filename)
    (cond
      ((null? (parser filename)) '())
      (else (runParsedList (parser filename))))))

; This function will run through the parsed list recursively using the runParse function to read what each
; part of the list should do accordingly. If the function reaches the null case then we are done running.
; Else recursively call the runParse on the first and rest of the list 
(define runParsedList
  (lambda (l)
    (cond
      ((null? l) '())
      ((null? (cdr l)) (runParse (car l)))
      (else (runParse (car l)) (runParsedList (cdr l))))))

; runParse will check to see what kind of statement we should be running 
(define runParse
  (lambda (l)
    (cond
      ((null? l) '())
      ((eq? (car l) 'var) (varDeclare (cdr l)))
      ((eq? (car l) '=) (assign (cdr l)))
      ((eq? (car l) 'return) (return (cdr l)))
      ((eq? (car l) 'if) (conditional (cdr l)))
      ((eq? (car l) 'while) (while (cdr l)))
      (else
       (error "Invalid Statement")))))

; This function will take in the declaration var statement and run it using pairs. If theres only a name then
; it gets put with an empty list or else it gets added to the state 
(define varDeclare
  (lambda (l)
    (cond
      ((null? (cdr l)) (addPair (car l) '()))
      (else (addPair (car l) (M_val_bool (car (cdr l))))))))

; This function simply assigns a val to a var or updates it if it exists in the state already 
(define assign
  (lambda (l)
    (cond
      ((null? l) '())
       ((isDefined (car l) State) (addPair (car l) (M_val_bool (car (cdr l)))))
       (else ((isDefined (car l) State) (error "Var not defined"))))))

; This function simply returns the var or evaluated expression, also switching the bool from "#t/#f" to "true/false" 
(define return
  (lambda (l)
    (cond
      ((null? l) (error "Cannot be an empty list"))
      ((eq? (M_val_bool (car l)) #t) 'true)
      ((eq? (M_val_bool (car l)) #f) 'false)
      (else (M_val_bool (car l))))))

; This function runs the conditional statements required by the state by evaluating the expression. It calls back
; to the runParse to continue through the conditional
(define conditional
  (lambda (l)
    (cond
      ((M_val_bool (car l)) (runParse (cadr l)))
      ((null? (cddr l)) '())
      (else (runParse (caddr l))))))

; This function simply loops through the given expression until our case is true
(define while
  (lambda (l)
    (cond
      ((not (M_val_bool (car l))) '())
      (else (runParse (car (cdr l))) (while l)))))

;----------------------------------------------------------------------------------------------------------------------
; The following are all state functions 

; The state will hold a list of all the pairs in the form (var val)
(define State '())

; Function used to make the state with a new pair
(define makeState
  (lambda (state pair) 
    (cond
      ((null? state) pair) 
      (else (cons (car state) (makeState (cdr state) pair))) )))

; Function to add pairs into the state by checking to see if its already in the state first, and if it is then
; it removes the old pair and adds the newly updated one. It also reassembles the state after removing the variable
; pair, essentially creating a brand new state 
(define addPair
  (lambda (var val)
    (set! State (makeState (removePair var State) 
                             (cons (cons var (cons val '())) '()) ))))

; Function to remove the first instance of a variable with a given name in the current state
(define removePair
  (lambda (var state)
    (cond
      ((null? state) '())
      ((eq? var (car (car state))) (cdr state))
      (else (cons (car state) (removePair var (cdr state)))) )))

; Function checks to see if the variables in the state is defined 
(define isDefined
  (lambda (var state)
    (cond
      ((null? state) #f)
      ((eq? var (car (car state))) #t)
      (else (isDefined var (cdr state))))))

; Function that gets val of a declared var in the state
(define getVal
  (lambda (a l)
    (cond
      ((null? l) '())
      ((eq? (car (car l)) a) (car (cdr (car l))))
      (else (getVal a (cdr l))))))

; Helper atom function
(define atom?
  (lambda (a)
    (and (not (pair? a)) (not (null? a)))))

; Function that runs through the expressions in the list and performs value and bool operations in order of presidence
(define M_val_bool
  (lambda (l)
    (cond
      ((null? l) '())
      ((number? l) l)
      ((eq? l 'true) #t)
      ((eq? l 'false) #f)
      ((atom? l) (if (isDefined l State) (if (not (null? (getVal l State))) (getVal l State) (error "Variable not assigned.")) (error "Variable not declared.")))
      ((eq? (car l) '+) (+ (M_val_bool (car (cdr l))) (M_val_bool (caddr l))))
      ((eq? (car l) '-) (if (null? (cddr l)) (* (M_val_bool (car (cdr l))) -1) (- (M_val_bool (car (cdr l))) (M_val_bool (caddr l)))))
      ((eq? (car l) '*) (* (M_val_bool (car (cdr l))) (M_val_bool (caddr l))))
      ((eq? (car l) '/) (quotient (M_val_bool (car (cdr l))) (M_val_bool (caddr l))))
      ((eq? (car l) '%) (modulo (M_val_bool (car (cdr l))) (M_val_bool (caddr l))))
      ((eq? (car l) '==) (eq? (M_val_bool (car (cdr l))) (M_val_bool (caddr l))))
      ((eq? (car l) '!=) (not (eq? (M_val_bool (car (cdr l))) (M_val_bool (caddr l)))))
      ((eq? (car l) '<) (< (M_val_bool (car (cdr l))) (M_val_bool (caddr l))))
      ((eq? (car l) '>) (> (M_val_bool (car (cdr l))) (M_val_bool (caddr l))))
      ((eq? (car l) '<=) (<= (M_val_bool (car (cdr l))) (M_val_bool (caddr l))))
      ((eq? (car l) '>=) (>= (M_val_bool (car (cdr l))) (M_val_bool (caddr l))))
      ((eq? (car l) '&&) (and (M_val_bool (car (cdr l))) (M_val_bool (caddr l))))
      ((eq? (car l) '||) (or (M_val_bool (car (cdr l))) (M_val_bool (caddr l))))
      ((eq? (car l) '!) (not (M_val_bool (car (cdr l))))))))