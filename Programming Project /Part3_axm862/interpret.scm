; An interpreter for a Java/C-like language. Uses functionParser.scm to parse input.
; EECS 345 Programming Language Concepts, Part 3
; Aditya Malik (axm862) and Ayush Karnawat (axk840)

(load "functionParser.scm")
(load "environment.scm")

; -----------------------------------------------------------------------------------------------------------------------
; The following are defined as our top level for the project, with default error throws used as parameters when breaking
;   down function calls. 

; Default interpret function call
(define interpret
  (lambda (filename)
    (return_val
     (let
         ((env (interpret_state (parser filename)
                                      (add_binding_to_state 'true #t
                                           (add_binding_to_state 'false #f (new_state)) )
                                      default_return
                                      default_continue
                                      default_break
                                      default_throw)) )
                  (call/cc
                    (lambda (return)
                      (interpret_value '(funcall main) env return default_continue default_break default_throw))) ))))

; Default return wrapper function
(define return_val
  (lambda (val)
    (cond
      ((eq? val #t) 'true)
      ((eq? val #f) 'false)
      ((eq? val 'undeclared) (error 'var "ERROR: Cannot use an undeclared variable."))
      ((eq? val 'uninitialized) (error "ERROR: Cannot use an uninitialized variable."))
      (else val) )))

; Default error returns
(define default_return
  (lambda (v)
    (error 'Invalid_Return: "ERROR: Cannot use return outside of the function call.")))

(define default_continue
  (lambda (v)
    (error 'Invalid_Continue: "ERROR: Cannot use continue outside of function loop.")))

(define default_break
  (lambda (v)
    (error 'Invalid_Break: "ERROR: Break was used outside of function loop.")))

(define default_throw
  (lambda (v)
    (error 'Unhandled_Exception: "ERROR: Thrown exception was not caught.")))

; -----------------------------------------------------------------------------------------------------------------------
; Similar to the 2nd part of the project, this is the interpreting value section where each value is checked and analyzed.
; The new part is the function call, and the rest is pretty much copied over from our second part. 

; Some default abstractions since we need them everywhere
(define operator car)
(define leftop cadr)
(define rightop caddr)
(define fname cadr)
(define fbody cadr)
(define listofpara cddr)
(define makeenvfunc caddr)
(define formalpara car)
(define actualpara cddr)
(define firstpara car)
(define restpara cdr)

; Function that checks to see what the value of the statement is and does the corresponding correct output
(define interpret_value
  (lambda (stmt env return continue break throw)
    (cond
      ((null? stmt) '())
      ((number? stmt) stmt)
      ((atom? stmt) (lookup_val stmt env))
      ((eq? 'funcall (operator stmt)) (M_value_funcall stmt env return continue break throw))
      ((null? (leftop stmt)) (interpret_value (car stmt) env return continue break throw))
      ((eq? '+ (operator stmt)) (+ (interpret_value (leftop stmt) env return continue break throw) (interpret_value (rightop stmt) env return continue break throw)))
      ((and (eq? '- (operator stmt)) (isSingleOper? stmt)) (- (interpret_value (leftop stmt) env return continue break throw)))
      ((eq? '- (operator stmt)) (- (interpret_value (leftop stmt) env return continue break throw) (interpret_value (rightop stmt) env return continue break throw)))
      ((eq? '* (operator stmt)) (* (interpret_value (leftop stmt) env return continue break throw) (interpret_value (rightop stmt) env return continue break throw)))
      ((eq? '/ (operator stmt)) (quotient (interpret_value (leftop stmt) env return continue break throw) (interpret_value (rightop stmt) env return continue break throw)))
      ((eq? '% (operator stmt)) (remainder (interpret_value (leftop stmt) env return continue break throw) (interpret_value (rightop stmt) env return continue break throw)))
      ((eq? '== (operator stmt)) (= (interpret_value (leftop stmt) env return continue break throw) (interpret_value (rightop stmt) env return continue break throw)))
      ((eq? '!= (operator stmt)) (not (= (interpret_value (leftop stmt) env return continue break throw) (interpret_value (rightop stmt) env return continue break throw))))
      ((eq? '< (operator stmt)) (< (interpret_value (leftop stmt) env return continue break throw) (interpret_value (rightop stmt) env return continue break throw)))
      ((eq? '> (operator stmt)) (> (interpret_value (leftop stmt) env return continue break throw) (interpret_value (rightop stmt) env return continue break throw)))
      ((eq? '<= (operator stmt)) (<= (interpret_value (leftop stmt) env return continue break throw) (interpret_value (rightop stmt) env return continue break throw)))
      ((eq? '>= (operator stmt)) (>= (interpret_value (leftop stmt) env return continue break throw) (interpret_value (rightop stmt) env return continue break throw)))
      ((eq? '|| (operator stmt)) (or (interpret_value (leftop stmt) env return continue break throw) (interpret_value (rightop stmt) env return continue break throw)))
      ((eq? '&& (operator stmt)) (and (interpret_value (leftop stmt) env return continue break throw) (interpret_value (rightop stmt) env return continue break throw)))
      ((eq? '! (operator stmt)) (not (interpret_value (leftop stmt) env return continue break throw)))
      (else (printf "ERROR: Value is invalid." )) )))
                               
; This part is newly added and interprets the value of a function call
(define M_value_funcall
  (lambda (stmt env return continue break throw)
    (let* ((closure (lookup_val (fname stmt) env))
           (outerenv ((makeenvfunc closure) env))
           (new_state (cons (newlayerpara (formalpara closure) (actualpara stmt) env return continue break throw) outerenv)))
      (call/cc
       (lambda (return)
         (interpret_stmt_list (fbody closure) new_state return default_continue default_break throw)))) ))

; ------------------------------------------------------------------------------------------------------------------------------
; Similar to the 2nd part of the project, these are our state function calls which do the actual operation depending on the call.
; The two new parts are the function and function call, and the rest is just copied over from the old part.

; Checks to see the actual state of the statement and where to send it off to 
(define interpret_state
  (lambda (stmt env return continue break throw)
    (cond
      ((null? stmt) env)
      ((atom? stmt) env)
      ((statement-list? stmt) (interpret_stmt_list stmt env return continue break throw))
      ((assignment? stmt) (interpret_assign stmt env return continue break throw))
      ((declaration? stmt) (interpret_declare stmt env return continue break throw))
      ((if? stmt) (interpret_if stmt env return continue break throw))
      ((while? stmt) (interpret_while stmt env return continue break throw))
      ((try? stmt) (interpret_try stmt env return continue break throw))
      ((block? stmt) (interpret_block stmt env return continue break throw))
      ((return? stmt) (interpret_return stmt env return continue break throw))
      ((throw? stmt) (throw (interpret_value (errVal stmt) env return continue break throw) env))
      ((function? stmt) (interpret_func stmt env return continue break throw))
      ((fcall? stmt) (interpret_funcall stmt env return continue break throw))
      ((continue? stmt) (continue env))
      ((break? stmt) (break env))
      (else env) )))

; Function that divides the input parse tree into readable statements
(define interpret_stmt_list
  (lambda (stmts env return continue break throw)
    (if (null? stmts) env (interpret_stmt_list (nextstmts stmts) (interpret_state (firststmt stmts) env return continue break throw) return continue break throw)) ))

; Interprets the assign statement
(define interpret_assign
  (lambda (stmt env return continue break throw)
    (update_val (var stmt) (interpret_value (expression stmt) env return continue break throw) env)))

; Interprets the declaration statement
(define interpret_declare
  (lambda (stmt env return continue break throw)
    (cond
      ((null? (inits stmt)) (add_binding_to_state (var stmt) 'uninitialized env))
      (else (add_binding_to_state (var stmt) (interpret_value (expression stmt) env return continue break throw) (interpret_state (expression stmt) env return continue break throw))) )))

; Interprets the if statement
(define interpret_if
  (lambda (stmt env return continue break throw)
    (cond
      ((interpret_value (condition stmt) env return continue break throw) (interpret_state (thenbranch stmt) env return continue break throw))
      ((haselsebranch? stmt) (interpret_state (elsebranch stmt) env return continue break throw))
      (else env))))

; Interprets the while statement
(define interpret_while
  (lambda (stmt env return continue break throw)
    (call/cc
     (lambda (new-break)
       (letrec
           ((loop (lambda (test body env)
                    (if (interpret_value test env return continue break throw)
                       (loop test body (call/cc (lambda (new-continue)
                                                   (interpret_state body env return new-continue new-break throw)))) env))))
         (loop (condition stmt) (whilebody stmt) env))))))

; Interprets the try statement
(define interpret_try
  (lambda (stmt env return continue break throw)
    (cond
      ((not (hascatch? stmt)) (interpret_state (finallybody stmt) (interpret_state (trybody stmt) return continue break throw) return continue break throw))
      ((not (hasfinally? stmt)) (call/cc
                                 (lambda (newthrow)
                                   (interpret_state (trybody stmt) env return continue break (lambda (e new_state) (newthrow (interpret_catch (catchbody stmt) e (errName stmt) env return continue break throw)))))))
      (else (interpret_state (finallybody stmt)
                     (call/cc
                      (lambda (newthrow)
                        (interpret_state (trybody stmt) env return continue break (lambda (e new_state) (newthrow (interpret_catch (catchbody stmt) e (errName stmt) env return continue break throw))))))
                     return break continue throw)))))

; Interprets the catch statement
(define interpret_catch
  (lambda (stmt errval errName env return continue break throw)
      (interpret_state stmt (add_binding_to_state errName errval env) return continue break throw)))

; Function that evaluates each state inside the block of code
(define interpret_block
  (lambda (stmt env return continue break throw)
    (removelayer (interpret_stmt_list (blockbody stmt) (add_layer env) return (lambda (s) (continue (removelayer s))) (lambda (s) (break (removelayer s))) (lambda (v s) (throw v (removelayer s))))) ))

; Interprets the return statement
(define interpret_return
  (lambda (stmt env return continue break throw)
    (return (interpret_value (returnval stmt) env return continue break throw))))

; These two are the new parts used for function/function calls
; Interprets the actual function
(define interpret_func
  (lambda (stmt env return continue break throw)
    (add_binding_to_state (fname stmt)
         (list (params stmt)
               (body stmt)
               (lambda (env)
                 (get_layers_for_func (fname stmt) env))) env)))
         
; Interprets the function call
(define interpret_funcall
  (lambda (stmt env return continue break throw)
    (interpret_value stmt env return continue break throw) env))

;--------------------------------------------------------------------------------------------------------------------
; The following are helper functions used throughout the code, with newlayerpara making a new layer for the interpret.

; Function chekcs to see if there is only a single operand
(define isSingleOper?
  (lambda (stmt)
    (null? (cddr stmt))))

; Checks to see if a statement is an atom 
(define atom?
  (lambda (stmt)
    (and
     (not (pair? stmt))
     (not (null? stmt)))))

; Create a new layer using the given parameters
(define newlayerpara
  (lambda (formal actual env return continue break throw)
    (cond
      ((and (null? formal) (null? actual)) (new_layer))
      ((or (null? formal) (null? actual)) (error 'Mismatched_Args "Mismatched arguments and the number of args is incorrect."))
      (else (add_binding_to_layer (firstpara formal) (box (interpret_value (firstpara actual) env return continue break throw)) (newlayerpara (restpara formal) (restpara actual) env return continue break throw))) )))

; ------------------------------------------------------------------------------------------------------------------------
; The following is a huge list of abstractions used in our state function calls, making it easier to call each part and
;   used to test to see what kind of statement we are working with. These are the same as our second part of the project.

(define firststmt car)
(define nextstmts cdr)
(define statement-list?
  (lambda (stmt)
    (list? (car stmt)) ))

; Checks to see what the operator is equal to 
(define operEq
  (lambda (stmt op)
    (eq? op (operator stmt)) ))

; Checks to see if operator is an assignment call 
(define assignment?
  (lambda (stmt)
    (operEq stmt '=) ))

; Checks to see if operator is a declaration call
(define declaration?
  (lambda (stmt)
    (operEq stmt 'var) ))

; Checks to see if operator is an if call 
(define if?
  (lambda (stmt)
    (operEq stmt 'if) ))

; Checks to see if operator is a while call
(define while?
  (lambda (stmt)
    (operEq stmt 'while) ))

; Checks to see if operator is a try statement
(define try?
  (lambda (stmt)
    (operEq stmt 'try) ))

; Checks to see if operator is a catch statement
(define catch?
  (lambda (stmt)
    (operEq stmt 'catch) ))

; Checks to see if operator is a finally statement
(define finally?
  (lambda (stmt)
    (operEq stmt 'finally) ))

; Checks to see if operator is a blocked statement
(define block?
  (lambda (stmt)
    (operEq stmt 'begin) ))

; Checks to see if operator is a return call
(define return?
  (lambda (stmt)
    (operEq stmt 'return) ))

; Checks to see if operator is a break statement
(define break?
  (lambda (stmt)
    (operEq stmt 'break) ))

; Checks to see if operator is a continue statement
(define continue?
  (lambda (stmt)
    (operEq stmt 'continue) ))

; Checks to see if operator is a throw stmt
(define throw?
  (lambda (stmt)
    (operEq stmt 'throw) ))

; Checks to see if operator is a function *new*
(define function?
  (lambda (stmt)
    (operEq stmt 'function) ))

; Checks to see if operator is a function call *new*
(define fcall?
  (lambda (stmt)
    (operEq stmt 'funcall) ))

; Abstractions to check the branches and conditions of the blocked statements 
(define blockbody cdr)
(define var cadr)
(define expression caddr)
(define condition cadr)
(define thenbranch caddr)
(define elsebranch cadddr)
(define params caddr)
(define body cadddr)

; Checks to see if it has an else branch 
(define haselsebranch?
  (lambda (stmt)
    (not (null? (cdddr stmt))) ))

; More simple abstractions 
(define inits cddr)
(define whilebody caddr)
(define returnval cadr)
(define trybody cadr)
(define errVal cadr)

; Checks the finally portion of the body
(define finallybody
  (lambda (stmt)
    (cadr (cadddr stmt))))

; Checks the catch portion of the body
(define catchbody
  (lambda (stmt)
    (cdr (cdaddr stmt))))

; Checks to see if the body has catch
(define hascatch?
  (lambda (stmt)
    (not (null? (caddr stmt)))))

; Checks to see if the body has a finally 
(define hasfinally?
  (lambda (stmt)
    (not (null? (cadddr stmt)))))

; Checks for an error name in the statement
(define errName
  (lambda (stmt)
    (caar (cdaddr stmt))))

; ---------------------------------------------------------------------------------------------
;(interpret "tests/test1.javaish")
;(interpret "tests/test2.javaish")
;(interpret "tests/test3.javaish")
;(interpret "tests/test4.javaish")
;(interpret "tests/test5.javaish")
;(interpret "tests/test6.javaish")
;(interpret "tests/test7.javaish")
;(interpret "tests/test8.javaish")
;(interpret "tests/test9.javaish")
;(interpret "tests/test10.javaish")
;(interpret "tests/test11.javaish")
;(interpret "tests/test12.javaish")
;(interpret "tests/test13.javaish")
;(interpret "tests/test14.javaish")
;(interpret "tests/test15.javaish")
;(interpret "tests/test16.javaish")
;(interpret "tests/test17.javaish")
;(interpret "tests/test18.javaish")
;(interpret "tests/test19.javaish")
;(interpret "tests/test20.javaish")