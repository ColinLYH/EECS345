; An interpreter for a Java-like language. Uses simpleParser.scm to parse the input.
; EECS 345 Programming Language Concepts Part 2
; Aditya Malik, Ayush Karnawat

(load "simpleParser.scm")
(load "environment.scm")

; --------------------------------------------- Global Variables -------------------------------------------
; The first value of the valuelist in the state list
; (define first car)

; The rest of the statement. This is predefined in Pretty Big, hence why it is commented
; (define rest cdr)

; The first element of the rest of the list. This is predefined in Pretty Big, hence why it is commented
; (define second cadr)

; The first elemet of the rest of the rest of the list. Predefined by Pretty Big, hence why it is commented
; (define third caddr)

; the initialization of a var
(define init cddr)

; else branch in if statements
(define else_branch cadddr)

; ---------------------------------------------- Main function ---------------------------------------------
; Parses the file, and evaluates the results.
; 
; Usage:
;   (interpret "test.javaish")

; Interprets a file and unparses it into the interpret_state function 
(define interpret
  (lambda (filename)
    (unparse
     (call/cc
      (lambda (ret)
        (interpret_state (parser filename) (newenv) ret (lambda (v) (error "ERROR: Not a valid continue")) (lambda (v) (error "ERROR: Not a valid break")) (lambda (v env) (error "Unexpected ERROR" v))))))))

; Helper function to unparse and error check to see validity 
(define unparse
  (lambda (x)
    (cond
      ((eq? x #t) 'true)
      ((eq? x #f) 'false)
      ((eq? x 'undeclared) (error "ERROR: Undeclared variable!"))
      ((eq? x 'uninitialized) (error "ERROR: Uninitialized variable!"))
      (else x))))

; Interprets a single statement from the full unparsed file 
(define interpret_state
  (lambda (stmt env ret cont break throw)
    (cond
      ((null? stmt) env)
      ((atom? stmt) env) 
      ((list? (first stmt)) (interpret_statement_list stmt env ret cont break throw))
      ((eq? '= (first stmt)) (interpret_assign stmt env ret cont break throw))
      ((eq? 'var (first stmt)) (interpret_declare stmt env ret cont break throw))
      ((eq? 'if (first stmt)) (interpret_if stmt env ret cont break throw))
      ((eq? 'while (first stmt)) (interpret_while stmt env ret cont break throw))
      ((eq? 'try (first stmt)) (interpret_try stmt env ret cont break throw))
      ((eq? 'begin (first stmt)) (interpret_block stmt env ret cont break throw))
      ((eq? 'return (first stmt)) (interpret_return stmt env ret cont break throw))
      ((eq? 'throw (first stmt)) (throw (second stmt) env))
      ((eq? 'break (first stmt)) (break env))
      ((eq? 'continue (first stmt))  (cont env))
      (else env))))

; Interprets a list of parsed statements and forms a tree  
(define interpret_statement_list
  (lambda (stmts env ret cont break throw)
    (if (null? stmts) env (interpret_statement_list (rest stmts) (interpret_state (first stmts) env ret cont break throw) ret cont break throw))))

; interprets an assign statement as "x = 5"
(define interpret_assign
  (lambda (stmt env ret cont break throw)
    (if (inState? (second stmt) env)
        (addtoEnv (second stmt) (interpret_values (third stmt) env) env)
        (error 'variableUndeclared "ERROR: Assigning before declaration"))))

; interprets a declaration statement in either "var x;" or "var x = 5". 
(define interpret_declare
  (lambda (stmt env ret cont break throw)
    (cond
      ((inState? (second stmt) env) (error 'variableAlreadyDeclared "ERROR: Variable already declared" (second stmt)))
      ((null? (init stmt)) (addtoEnv (second stmt) 'uninitialized env))
      (else (addtoEnv (second stmt) (interpret_values (third stmt) env) (interpret_state (third stmt) env ret cont break throw))))))

; Interprets the if statement. It'll first check to see if the statement is declared in the state and if it exists in the environment. After that it'll get the values of each item in the if statements,
; and if it includes an else statement then it'll continue through pushing in the rest of the statement back into the interpret_state call. 
(define interpret_if
  (lambda (stmt env ret cont break throw)
    (cond
      ((isDeclared? stmt env) (error 'variableUndeclared "ERROR: Undeclared variable"))
      ((isInit? stmt env) (error 'variableUninitialized "ERROR: Uninitialized variable"))
      ((interpret_values (second stmt) env) (interpret_state (third stmt) env ret cont break throw))
      ((hasElse? stmt) (interpret_state (else_branch stmt) env ret cont break throw))
      (else env))))

; Interprets the while statement. It'll first check to see if the statement is declared in the state and if it exists in the environment. If it passes those tests, then it continues and loops into the while
; statement. Using call/cc we loop through the body of the statement and using interpret_state we get the associated values for each item in the parse tree. 
(define interpret_while
  (lambda (stmt env ret cont break throw)
    (cond
      ((isDeclared? stmt env) (error 'variableUndeclared "ERROR: Variable undeclared"))
      ((isInit? stmt env) (error 'variableUninitialized "ERROR: Variable not initialized"))
      (else
       (call/cc
        (lambda (new-break)
          (letrec
              ((loop (lambda (test body env)
                       (if (interpret_values test env)
                           (loop test body (call/cc
                                            (lambda (new-cont)
                                                      (interpret_state body env ret new-cont new-break throw)))) env))))
            (loop (second stmt) (third stmt) env))))))))

; The try function will check to see what types of statements (i.e. catch/finally) come after it.
; If both statements are avaiable, then we simply call interpret_state function on those specific bodies.
; However, if only catch is available as the next statement, then we only call interpret_state only on the body of the catch statement.
;   Similarly, if only the finally block is available, then we call the interpret_state on the body of the finally statement. 
(define interpret_try
  (lambda (stmt env ret cont break throw)
      (interpret_state (checkFinallyBody stmt) (interpret_state (checkBody stmt) env ret cont break (lambda (e newenv)
                                                                                         (interpret_state (checkFinallyBody stmt) (interpret_catch (checkCatchBody stmt) e (errorType stmt) newenv ret cont break throw) ret cont break throw))) ret cont break throw)))

; This is a helper method used by interpret_try to evaluate the catch block. 
(define interpret_catch
  (lambda (stmt error errorType env ret cont break throw)
      (interpret_state stmt (addtoEnv errorType error env) ret cont break throw)))

; Interprets a block of the statement input by removing a layer at a time and reading it 
(define interpret_block
  (lambda (stmt env ret cont break throw)
    (pop_layer (interpret_statement_list (rest stmt) (push_layer env) ret (lambda (s) (cont (pop_layer s))) (lambda (s) (break (pop_layer s))) (lambda (v s) (throw v (pop_layer s))))) ))

; Interprets the return statement
(define interpret_return
  (lambda (stmt env ret cont break throw)
    (ret (interpret_values (second stmt) env))))

; Interprets the value of the arithmic 
(define interpret_values
  (lambda (stmt env)
    (cond
      ((null? stmt) '())
      ((number? stmt) stmt)
      ((not (list? stmt)) (lookup_list stmt env))
      ((null? (rest stmt)) (interpret_values (first stmt) env))
      ((or (eq? 'uninitialized (interpret_values (second stmt) env)) (eq? 'undeclared (interpret_values (second stmt) env))) (error "ERROR: Variable not declared or initialized"))
      ((and (not (null? (cddr stmt))) (or (eq? 'uninitialized (interpret_values (third stmt) env)) (eq? 'undeclared (interpret_values (third stmt) env)))) (error "ERROR: Variable not declared or initialized!"))                                                                                                                                                                                                    
      ((eq? '+ (first stmt)) (+ (interpret_values (second stmt) env) (interpret_values (third stmt) env)))
      ((and (eq? '- (first stmt)) (null? (cddr stmt)))  (- (interpret_values (second stmt) env)))
      ((eq? '- (first stmt)) (- (interpret_values (second stmt) env) (interpret_values (third stmt) env)))
      ((eq? '* (first stmt)) (* (interpret_values (second stmt) env) (interpret_values (third stmt) env)))
      ((eq? '/ (first stmt)) (quotient (interpret_values (second stmt) env) (interpret_values (third stmt) env)))
      ((eq? '% (first stmt)) (remainder (interpret_values (second stmt) env) (interpret_values (third stmt) env)))
      ((eq? '== (first stmt))(= (interpret_values (second stmt) env) (interpret_values (third stmt) env)))
      ((eq? '!= (first stmt))(not (= (interpret_values (second stmt) env) (interpret_values (third stmt) env))))
      ((eq? '< (first stmt))(< (interpret_values (second stmt) env) (interpret_values (third stmt) env)))
      ((eq? '> (first stmt))(> (interpret_values (second stmt) env) (interpret_values (third stmt) env)))
      ((eq? '<= (first stmt))(<= (interpret_values (second stmt) env) (interpret_values (third stmt) env)))
      ((eq? '>= (first stmt))(>= (interpret_values (second stmt) env) (interpret_values (third stmt) env)))
      ((eq? '|| (first stmt))(or (interpret_values (second stmt) env) (interpret_values (third stmt) env)))
      ((eq? '&& (first stmt))(and (interpret_values (second stmt) env) (interpret_values (third stmt) env)))
      ((eq? '! (first stmt))(not (interpret_values (second stmt) env)))
      (else (error "Invalid Expression")))))

; ---------------------------------------------- Helper Functions ---------------------------------------------
; We didn't write abstractions for the cdaddr, cadddr, cdddr, caar, and cadr since they were only used once in the
; bottom helper functions. If we need them again for the next part of project 3 we will abstract them, but since they
; are already used in helper functions it didn't make any sense to.

; Determines whether is statement is neither null or a pair.
(define atom?
  (lambda (stmt)
    (and (not (pair? stmt)) (not (null? stmt))) ))

; Checks to see if a statement is declared in the environment
(define isDeclared?
  (lambda (stmt env)
    (eq? 'undeclared (interpret_values (second stmt) env)) ))

; Checks to see if a variable is already in the environment
(define inState?
  (lambda (var env)
    (not (eq? 'undeclared (lookup_list var env))) ))

; Checks to see if the statement is in the environment 
(define isInit?
  (lambda (stmt env)
    (eq? 'uninitialized (interpret_values (second stmt) env)) ))

; Grabs error type
(define errorType
  (lambda (stmt)
    (caar (cdaddr stmt))))

; Checks to see if the statement has an else portion
(define hasElse?
  (lambda (stmt)
    (not (null? (cdddr stmt))) ))

; Defines the body of the statement
(define checkBody cadr)

; Checks to see for the finally in the body 
(define checkFinallyBody
  (lambda (stmt)
    (checkBody (cadddr stmt))))

; Checks to see for the catch in the body 
(define checkCatchBody
  (lambda (stmt)
    (rest (cdaddr stmt))))