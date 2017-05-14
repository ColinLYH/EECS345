; The state will hold a list of all the pairs in the form (var val)
(define State '())

; Function used to set the state
(define setState
  (lambda (s)
    (set! State s)))

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
    (setState (makeState (removePair var State) 
                             (cons (cons var (cons val '())) '()) ))))

; Function to remove the first instance of a variable with a given name in the current state
(define removePair
  (lambda (var state)
    (cond
      ((null? state) '())
      ((eq? var (car (car state))) (cdr state))
      (else (cons (car state) (removePair var (cdr state)))) )))

; Function checks to see if the variables in the state is declared
(define declared
  (lambda (var)
    (recursiveDeclared var State)))

; Helper function that recursively checks for variable declaration. Base case checks to see if variable is in the state.
; Next if its found, we return true, else we recurse.
(define recursiveDeclared
  (lambda (var state)
    (cond
      ((null? state) #f)
      ((eq? var (car (car state))) #t)
      (else (recursiveDeclared var (cdr state))))))

; Function that finds val of a declared var in the state
(define valOf
  (lambda (a l)
    (cond
      ((null? l) '())
      ((eq? (caar l) a) (cadar l))
      (else (valOf a (cdr l))))))

; Helper atom function
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; Function that runs through the expressions in the list and performs value and bool operations
(define M_val_bool
  (lambda (l)
    (cond
      ((null? l) '())
      ((number? l) l)
      ((eq? l 'true) #t)
      ((eq? l 'false) #f)
      ((atom? l) (if (declared l) (if (not (null? (valOf l State))) (valOf l State)
                                                                   (error "Variable is not assigned!"))
                                   (error "Variable was not declared")))
      ((eq? (car l) '+) (+ (M_val_bool (cadr l)) (M_val_bool (caddr l))))
      ((eq? (car l) '-) (if (null? (cddr l)) (* (M_val_bool (cadr l)) -1)
                                             (- (M_val_bool (cadr l)) (M_val_bool (caddr l))) ))
      ((eq? (car l) '*) (* (M_val_bool (cadr l)) (M_val_bool (caddr l))))
      ((eq? (car l) '/) (quotient (M_val_bool (cadr l)) (M_val_bool (caddr l))))
      ((eq? (car l) '%) (modulo (M_val_bool (cadr l)) (M_val_bool (caddr l))))
      ((eq? (car l) '==) (eq? (M_val_bool (cadr l)) (M_val_bool (caddr l))))
      ((eq? (car l) '!=) (not (eq? (M_val_bool (cadr l)) (M_val_bool (caddr l)))))
      ((eq? (car l) '<) (< (M_val_bool (cadr l)) (M_val_bool (caddr l))))
      ((eq? (car l) '>) (> (M_val_bool (cadr l)) (M_val_bool (caddr l))))
      ((eq? (car l) '<=) (<= (M_val_bool (cadr l)) (M_val_bool (caddr l))))
      ((eq? (car l) '>=) (>= (M_val_bool (cadr l)) (M_val_bool (caddr l))))
      ((eq? (car l) '&&) (and (M_val_bool (cadr l)) (M_val_bool (caddr l))))
      ((eq? (car l) '||) (or (M_val_bool (cadr l)) (M_val_bool (caddr l))))
      ((eq? (car l) '!) (not (M_val_bool (cadr l))))
      (else (if (declared? (car l)) (valOf (car l) State) (error ("Variable was not declared!")))))))