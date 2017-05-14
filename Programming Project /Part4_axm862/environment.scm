; An interpreter for a Java/C-like language. Uses classParser.scm to parse input.
; EECS 345 Programming Language Concepts, Part 4
; Aditya Malik (axm862) and Ayush Karnawat (axk840)

; --------------------------------------------------------------------------------------------------
; The following are the layer operations used to modify the layer and their bindings.

; Needed abstractions 
(define firstvar caar)
(define firstval caadr)
(define varlist car)
(define vallist cadr)

; Construct a new layer
(define newlayer
  (lambda () '(() ())))

; Add a layer to the environment 
(define addtolayer
  (lambda (layer var value)
    (list (cons var (varlist layer)) (append (vallist layer) (list value)))))

; Add an environment 
(define add_env
  (lambda (env var value)
    (addtolayer env var (box value))))

; Look up a variable in the layer 
(define lookuplayer
  (lambda (layer var)
    (let ((index (ret_index (varlist layer) var)))
      (cond
        ((= index -1) 'no_such_value)
        (else (list-ref (vallist layer) (- (length (varlist layer)) index 1)))) )))

; Look up a value in the environment 
(define lookup_env
  (lambda (env var)
    (let ((value (lookuplayer env var)))
      (cond
        ((eq? value 'no_such_value) 'no_such_value)
        (else (unbox value)) ))))

; Function to check the variables in the layers
(define checklayers
  (lambda (layer var)
    (not (= -1 (ret_index (car layer) var)))))


; --------------------------------------------------------------------------------------------------
; The following are the environment operations used to modify the layers used in the code

; Needed abstractions 
(define removelayer cdr) ; Removes the top layer from the list of layers
(define getlayer car) ; Gets the first layer 
(define remaininglayer cdr) ; Gets the rest of the layers 

; Create a new state 
(define newstate
  (lambda () '((() ()))))

; Add a new layer to the layer on top of the current layers
(define addlayer
  (lambda (env)
    (cons (newlayer) env)))

; Add a binding to a state
(define addbindingtostate
  (lambda (env var value)
    (cond
      ((checklayers (car env) var) (error "ERROR. Variable already declared!"))
      (else (cons (addtolayer (getlayer env) var (box value)) (remaininglayer env))) )))

; Returns the variable binded in the env
(define ret_binding
  (lambda (env var)
    (cond
      ((null? env) 'no_such_value)
      (else (let ((value (lookuplayer (getlayer env) var)))
              (cond
                ((eq? value 'no_such_value) (ret_binding (remaininglayer env) var))
                (else value) ))) )))

; gets env for a function
(define ret_env_from_func
  (lambda (fname env)
    (cond
      ((null? env) (error "ERROR. Function name was not found!"))
      (else (let ((val (lookuplayer (car env) fname)))
               (cond
                 ((eq? val 'no_such_value)  (ret_env_from_func fname (cdr env)))
                 (else env) ))) )))

; Checks to see if the box is in the stae
(define check_boxState
  (lambda (env var)
    (let ((value (ret_binding env var)))
      (cond
        ((eq? value 'no_such_value) (error "ERROR. Variable binding was not found!"))
        (else value)) )))

; Look up the binding in the state
(define check_bindState
  (lambda (env var)
    (unbox (check_boxState env var))))

; Checks to see if the state has the member
(define checkStateMember
  (lambda (env var)
    (not (eq? (ret_binding env var) 'no_such_value)) ))

;--------------------------------------------------------------------------------------------------------------------
; The following are helper functions used throughout the code.

; Default abstractions 
(define name_item car)
(define value_item cdr)
(define value_list cdr)
(define name_list car)
(define object car)
(define expr caddr)
(define vartype car)

; Checks to see if its an atom, we couldn't find a way not to use this
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x))) ))

; Checks to see if two values are equal
(define ==
  (lambda (x y)
    (cond
     ((and (number? x) (number? y)) (= x y))
     ((and (atom? x) (atom? y)) (eqv? x y))
     ((and (list? x) (list? y)) (equal? x y))
     (else #f) )))

; Checks to see if two values are NOT equal 
(define !=
  (lambda (x y)
    (not (== x y)) ))

; Return an atoms index contained in the list
(define ret_index
  (lambda (list item)
    (((lambda (f) (f f))
      (lambda (f)
        (lambda (list item return)
          (cond
           ((null? list) -1) ;; No CPS return
           ((eq? (name_item list) item) (return 0))
           (else ((f f) (value_item list) item (lambda (v) (return (+ 1 v)))))))))
     list item (lambda (v) v))))

; Replaces the index value in the list with the input value 
(define replaceVal
  (lambda (list index value)
    (cond
      ((= 0 index) (cons value (value_list list)))
      (else (cons (name_list list) (replaceVal (value_list list) (- index 1) value))) )))

; Function that changes a statement from t-> true and f->false
(define func_change
  (lambda (stmt)
    (cond
      ((eq? stmt #t) 'true)
      ((eq? stmt #f) 'false)
      (else stmt))))