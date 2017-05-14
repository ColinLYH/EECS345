; The environment for that contains all the active bindings in the scope.
; EECS 345 Programming Language Concepts, Part 3
; Aditya Malik, Ayush Karnawat

; --------------------------------------------------------------------------------------------------
; The following are the layer operations used to modify the layer and their bindings.

; Basic functions used for abstraction
(define list_vars car)
(define first_var caar)
(define rest_vars cdar)

(define list_vals cadr)
(define first_val caadr)
(define rest_vals cdadr)

; Construct a new layer
(define new_layer
  (lambda () '(() ())))

; Get the layer with the first binding removed
(define get_rest_bindings_in_layer
  (lambda (layer)
    (list (rest_vars layer) (rest_vals layer)) ))

; Check if the layer is empty
(define empty_layer?
  (lambda (layer)
    (null? (list_vars layer)) ))

; Add a binding to the layer
(define add_binding_to_layer
  (lambda (var val layer)
    (list (cons var (list_vars layer)) (cons val (list_vals layer))) ))

; Lookup a variable in a layer
(define lookup_var_layer
  (lambda (var layer)
    (cond
      ((empty_layer? layer) 'undeclared)
      ((eq? var (first_var layer)) (first_val layer))
      (else (lookup_var_layer var (get_rest_bindings_in_layer layer))) )))

; Check if the layer contains the variable
(define layer_contains_var?
  (lambda (var layer)
    (not (eq? (lookup_var_layer var layer) 'undeclared)) ))

; --------------------------------------------------------------------------------------------------
; The following are the environment operations used to modify the environment and their bindings.

(define first_layer car)
(define rest_layers cdr)
(define new_state
  (lambda () '((() ()))))

; Add a new layer to the layer on top of the current layers
(define add_layer
  (lambda (state)
    (cons (new_layer) state) ))

; Removes the top layer from the list of layers
(define removelayer cdr)

; Add a binding to a state
(define add_binding_to_state
  (lambda (var val state)
    (cond
      ((layer_contains_var?  var (first_layer state)) (error 'Variable_Already_Declared: var " has already been declared!"))
      (else (cons (add_binding_to_layer var (box val) (first_layer state)) (rest_layers state))) )))

; Look up a variable in a state
(define lookup_var_state
  (lambda (var state)
    (cond
      ((null? state) 'undeclared)
      ((layer_contains_var? var (first_layer state)) (lookup_var_layer var (first_layer state)))
      (else (lookup_var_state var (rest_layers state))) )))

; Check if the state contains the variable
(define state_contains_var?
  (lambda (var state)
    (not (eq? (lookup_var_state var state) 'undeclared)) ))

; Return the layer of the environment of a function being called
(define get_layers_for_func
  (lambda (function_name state)
    (cond
      ((null? state) (error 'undefinedFunction: function_name " is undefined! "))
      ((layer_contains_var? function_name (first_layer state)) state)
      (else (get_layers_for_func function_name (rest_layers state))) )))

; Get the box of the variables
(define get_box_vars_from_state
  (lambda (var state)
    (cond
      ((not (state_contains_var? var state)) (error 'Undeclared_Variable "Attempted to use variable without declaration! Variable may be out of scope."))
      ((eq? 'uninitialized (lookup_var_state var state)) (error 'Uninitialized_Variable: var " used without being initialized!"))
      (else (lookup_var_state var state)) )))
              
; Lookup the value of a variable in the list
(define lookup_val
  (lambda (var state)
    (unbox (get_box_vars_from_state var state)) ))

; Update the value of a variable in the list
(define update_val
  (lambda (var val state)
    (cond
      ((state_contains_var? var state) (begin (set-box! (lookup_var_state var state) val) state))
      (else (error 'Undeclared_Variable "Attempted to use variable without declaration! Variable may be out of scope!")) )))