; An interpreter for a Java/C-like language. Uses classParser.scm to parse input.
; EECS 345 Programming Language Concepts, Part 4
; Aditya Malik (axm862) and Ayush Karnawat (axk840)

; Load the required files
(load "classParser.scm")
(load "environment.scm")

; -----------------------------------------------------------------------------------------------------------------------
; The following are defined as our top level for the project, with default error throws used as parameters when breaking
;   down function calls. 

; Default interpret function call, which now takes a file and a class and parses it 
(define interpret
  (lambda (filename class)
    (func_change
     (let
         ((env (interpret_outer (parser filename) (newstate) (ctx_default))))
       (call/cc
        (lambda (return)
          (interpret_value (list 'funcall (list 'dot class 'main)) env (ctx_ret (ctx_default) return)))) ))))

; Interpreting the outer function scope for the class             
(define interpret_outer
  (lambda (block env ctx)
    (cond
     ((null? block) env)
     ((eq? (car (car block)) 'class) (interpret_outer (cdr block) (M_state_class (car block) env ctx) ctx))
     (else (error "ERROR. Scope is not valid for the class!")) )))


; -------------------------------------------------------------------------------------------------------------
; The following are the context operations we used. More specifically, we named them "ctx" 

; Needed abstractions
(define ret_ctx car)
(define cont_ctx cadr)
(define break_ctx caddr)
(define throw_ctx cadddr)
(define class_ctx (lambda (l) (list-ref l 4)))
(define instClass_ctx (lambda (l) (list-ref l 5)))
(define curClass_ctx (lambda (l) (list-ref l 6)))

; These are the defalt error catches for the given operation
(define defaultreturn
	(lambda (v)
         (error 'invalidReturn: "ERROR. The return was used outside of function!")))
(define defaultcontinue 
	(lambda (v)
         (error 'invalidContinue: "ERROR. The continue was used outside of loop!")))
(define defaultbreak 
	(lambda (v)
	 (error 'invalidBreak: "ERROR. The break was used outside of loop!")))
(define defaultthrow 
	(lambda (v w)
	 (error 'unhandledException: "ERROR. The exception was thrown and not caught!")))

(define ctx_default
  (lambda ()
    (list defaultreturn defaultcontinue defaultbreak defaultthrow 'null 'null 'null)))
; Numbers used for index catching
(define ctx_ret
  (lambda (ctx return)
    (cons return (cdr ctx))))

(define setCtx_cont
  (lambda (ctx break)
    (replaceVal ctx 1 break)))

(define setCtx_break
  (lambda (ctx continue)
    (replaceVal ctx 2 continue)))

(define setCtx_throw
  (lambda (ctx class)
    (replaceVal ctx 3 class)))

(define setCtx_class
  (lambda (ctx instclass)
    (replaceVal ctx 4 instclass)))

(define setCtx_instClass
  (lambda (ctx throw)
    (replaceVal ctx 5 throw)))

(define setCtx_curClass
  (lambda (ctx currclass)
    (replaceVal ctx 6 currclass)))


; -----------------------------------------------------------------------------------------------------------------------
; Similar to the 2nd part of the project, this is the value section where each value is checked and analyzed.

; Some default abstractions since we need them everywhere
(define oper car)
(define leftop cadr)
(define rightop caddr)
(define formalpara car)
(define actualpara cddr)
(define firstpara car)
(define restpara cdr)
(define fname cadr)
(define fbody cadr)
(define binding cadr)

; Function chekcs to see if there is only a single operand for binary calls
(define binaryOpInterpret
  (lambda (oper)
    (cond
      ((eq? oper '+) +)
      ((eq? oper '-) -)
      ((eq? oper '/) quotient)
      ((eq? oper '*) *)
      ((eq? oper '%) remainder)
      ((eq? oper '&&) (lambda (x y) (and x y)))
      ((eq? oper '||) (lambda (x y) (or x y)))
      ((eq? oper '<) <)
      ((eq? oper '>) >)
      ((eq? oper '<=) <=)
      ((eq? oper '>=) >=)
      ((eq? oper '==) ==)
      ((eq? oper '!=) !=)
      (else (error "ERROR. Operator not valid!")) )))

; Function chekcs to see if there is only a single operand
(define isSingleOper?
  (lambda (oper)
    (cond
      ((eq? oper '!) not)
      ((eq? oper '-) (lambda (x) (- 0 x)))
      (else (error "ERROR. Invalid operator: " oper)))))

; Interprets the expr statement 
(define M_value_expression
  (lambda (stmt env ctx)
    (cond
      ((= 3 (length stmt)) ((binaryOpInterpret (oper stmt)) (interpret_value (leftop stmt) env ctx) (interpret_value (rightop stmt) env ctx)))
      (else ((isSingleOper? (oper stmt)) (interpret_value (leftop stmt) env ctx))) )))

; Interprets the assign statement
(define M_value_assign
  (lambda (stmt env ctx)
    (let* ((box (searchVars (leftop stmt) env ctx))
           (value (interpret_value (rightop stmt) env ctx)) )
      (set-box! box value)
      value)))

; Interprets the atom statement 
(define M_value_atom
  (lambda (stmt env ctx)
    (cond
      ((or (boolean? stmt) (number? stmt)) stmt)
      ((eq? stmt 'true) #t)
      ((eq? stmt 'false) #f)
      ((eq? 'undefined (M_value_var stmt env ctx)) (error "ERROR. Use of undefined variable: " stmt))
      (else (M_value_var stmt env ctx)) )))

; Function that converts our given list to a layer for us to operate on
(define conversionListLayer
  (lambda (formal actual env ctx)
    (cond
     ((and (null? formal) (null? actual)) (newlayer))
     ((or (null? formal) (null? actual)) (error "ERROR. Argument numbers not matched!"))
     ((eq? '& (firstpara formal)) (addtolayer (conversionListLayer (actualpara formal) (restpara actual) env ctx)
                                              (cadr formal)
                                              (searchVars (firstpara actual) env ctx) ))
     (else (addtolayer (conversionListLayer (restpara formal) (restpara actual) env ctx)
                       (firstpara formal)
                       (box (interpret_value (firstpara actual) env ctx)) )) )))

; This part interprets the value of a function call
(define M_value_funccall
  (lambda (funccall env ctx)
    (let* ((l (searchFunc (fname funccall) env ctx))
           (closure (car l))
           (instance (cadr l))
           (instance ((list-ref closure 4) instance))
           (class (caddr l))
           (currclass ((cadddr closure) env))
           (class (if (eq? 'null instance) currclass class))
           (outerenv ((caddr closure) env))
           (newstate (cons (conversionListLayer (car closure) (cddr funccall) env ctx) outerenv))
           (err (lambda (v) (error "ERROR. Illegal break/continue!"))) )
      (call/cc
       (lambda (return)
         (interpret_stmt (fbody closure) newstate (setCtx_class (ctx_ret (setCtx_break (setCtx_instClass (setCtx_curClass (setCtx_cont ctx err) currclass) instance) err) return) class) ))) )))

; Returns the value function for the variable statement
(define M_value_var
  (lambda (stmt env ctx)
    (let ((box (box_varSearch stmt env (curClass_ctx ctx) (instClass_ctx ctx))))
      (cond
        ((eqv? box 'no_such_value) (error "ERROR. Variable not found: " stmt))
        (else (unbox box)) ))))

; Returns the value function for the dot statement
(define M_value_dot
  (lambda (stmt env ctx)
    (unbox (searchDotVar stmt env ctx)) ))

; Returns the value function for the new statement
(define M_value_new
  (lambda (stmt env ctx)
    (let ((class (check_bindState env (binding stmt))))
      (cond
        ((not (and (list? class) (eq? (car class) 'class))) (error "ERROR. Invalid class: " (cadr stmt)))
        (else (setInstant_vals (newinstance class) (box_list (binding (instanceOfClass class))))) ))))

; Function that boxes a whole list. Helper for M_value_new 
(define box_list
  (lambda (list)
    (map box list)))

; Returns the mathematical value result of a statement
(define interpret_value
  (lambda (stmt env ctx)
    (cond
      ((and (list? stmt) (eq? '= (oper stmt))) (M_value_assign stmt env ctx))
      ((and (list? stmt) (eq? 'funcall (oper stmt))) (M_value_funccall stmt env ctx))
      ((and (list? stmt) (eq? 'dot (oper stmt))) (M_value_dot stmt env ctx))
      ((and (list? stmt) (eq? 'new (oper stmt))) (M_value_new stmt env ctx))
      ((list? stmt) (M_value_expression stmt env ctx))
      (else (M_value_atom stmt env ctx)) )))

; ------------------------------------------------------------------------------------------------------------------------------
; Similar to the 2nd part of the project, these are our state function calls which do the actual operation depending on the call.

; Some default abstractions since we need them everywhere
(define var cadr)
(define expr caddr)
(define condition cadr)
(define whilebody caddr)
(define trybody cadr)
(define firststmt car)
(define reststmt cdr)
(define params caddr)
(define body cadddr)
(define catchblock caddr)
(define finallyblock cadddr)

; Checks to see the actual state of the statement and where to send it off to 
(define interpret_state
  (lambda (stmt env ctx)
    (cond
     ((null? stmt) env)
     ((list? stmt) (cond
                    ((list? (oper stmt)) (interpret_stmt stmt env ctx))
                    ((eq? 'begin (oper stmt)) (interpret_block stmt env ctx))
                    ((eq? 'var (oper stmt)) (interpret_declare stmt env ctx))
                    ((eq? '= (oper stmt)) (interpret_assign stmt env ctx))
                    ((eq? 'if (oper stmt)) (interpret_if stmt env ctx))
                    ((eq? 'return (oper stmt)) (interpret_return stmt env ctx))
                    ((eq? 'while (oper stmt)) (interpret_while stmt env ctx))
                    ((eq? 'break (oper stmt)) ((break_ctx ctx) env))
                    ((eq? 'continue (oper stmt)) ((cont_ctx ctx) env))
                    ((eq? 'function (oper stmt)) (interpret_funcdec stmt env ctx))
                    ((eq? 'funcall (oper stmt)) (interpret_fcall stmt env ctx))
                    ((eq? 'try (oper stmt)) (interpret_try stmt env ctx))
                    ((eq? 'throw (oper stmt)) (interpret_throw stmt env ctx))
                    (else env)))
     (else env))))

; Function that divides the input parse tree into readable statements
(define interpret_stmt
  (lambda (block env ctx)
    (cond
      ((null? block) env)
      (else (interpret_stmt (reststmt block) (interpret_state (firststmt block) env ctx) ctx)) )))

; Function that evaluates each state inside the block of code
(define interpret_block
  (lambda (block env ctx)
    (removelayer
     (interpret_stmt (reststmt block) (addlayer env)(setCtx_cont (setCtx_break ctx (lambda (s) ((break_ctx ctx) (removelayer s)))) (lambda (s) ((cont_ctx ctx) (removelayer s))) ) ) )))

; Interprets the declaration statement
(define interpret_declare
  (lambda (stmt env ctx)
    (cond
      ((= (length stmt) 3) (addbindingtostate (interpret_state (expr stmt) env ctx) (var stmt) (interpret_value (expr stmt) env ctx))) 
      (else (addbindingtostate env (var stmt) 'undefined)) )))

; Interprets the assign statement
(define interpret_assign
  (lambda (stmt env ctx)
    (begin (M_value_assign stmt env ctx) env) ))

; Interprets the if statement
(define interpret_if
  (lambda (stmt env ctx)
    (cond
      ((= 3 (length stmt))
       (cond
         ((interpret_value (list-ref stmt 1) env ctx) (interpret_state (list-ref stmt 2) env ctx))
         (else env) ))
      (else
       (cond
         ((interpret_value (list-ref stmt 1) env ctx) (interpret_state (list-ref stmt 2) env ctx))
         (else (interpret_state (list-ref stmt 3) env ctx)) )) )))

; Interprets the return statement
(define interpret_return
  (lambda (stmt env ctx)
    ((ret_ctx ctx) (interpret_value (var stmt) env ctx)) ))

; Interprets the while statement
(define interpret_while
  (lambda (stmt env ctx)
    (call/cc
     (lambda (break_new)
       (letrec
           ((loop (lambda (condition body env)
                    (cond
                      ((interpret_value condition env (setCtx_break ctx break_new))
                       (loop condition body
                             (call/cc (lambda (continue_new)
                                        (interpret_state body env (setCtx_cont (setCtx_break ctx break_new) continue_new)) ))))
                      (else env) ))))
         (loop (condition stmt) (whilebody stmt) env) )))))

; Interprets the actual function
(define interpret_funcdec
  (lambda (funcDeclare env ctx)
    (let ((fname (cadr funcDeclare)))
      (addbindingtostate env fname
                 (list (params funcDeclare) 
                       (body funcDeclare) 
                       (lambda (env) (ret_env_from_func fname env))
                       (lambda (env) (class_ctx ctx))
                       (lambda (v) v) ))) ))
                       
; Interprets the function call
(define interpret_fcall
  (lambda (funccall env ctx)
    (begin (interpret_value funccall env ctx) env)))

; Interprets the try statement and returns the environment
(define interpret_try
  (lambda (stmt env ctx)
    (let* ((finally (makeFinally (finallyblock stmt) env ctx))
           (context2 (contextUpdate ctx finally)))
      (finally
       (call/cc
        (lambda (c)
          (let* ((catch (makeCatch (catchblock stmt) finally c env context2))
                 (newcontext (setCtx_throw context2 catch)))
            (interpret_block (cons 'begin (trybody stmt)) env newcontext)))))) ))

; Creates the finally statement for context 
(define makeFinally
  (lambda (l env ctx)
    (cond
      ((null? l) (lambda (thrown) thrown))
      (else (lambda (thrown)
              (begin (interpret_block (cons 'begin (cadr l)) env ctx) thrown))) )))

; Creates the make statement for the context 
(define makeCatch
  (lambda (l finally continuation env ctx)
    (cond
      ((null? l) (throw_ctx ctx)) 
      (else (lambda (thrown)
              (continuation (interpret_stmt (params l) (cons (addtolayer (newlayer) (caadr l) (box thrown)) env) ctx)))) )))

; Interprets the throw statement
(define interpret_throw
  (lambda (stmt env ctx)
    ((throw_ctx ctx) (interpret_value (var stmt) env ctx)) ))
      
; Binds a class declaration to the name of the class in the env. 
(define M_state_class
  (lambda (stmt env ctx)
    (let* ((name (var stmt))
           (extends (expr stmt))
           (parent (if (null? extends) 'null (check_bindState env (binding extends))))
           (body (body stmt))
           (init (newClass parent name))
           (class (M_class_stmt body env (setCtx_curClass (setCtx_class ctx init) init))))
      (addbindingtostate env name class)) ))      

; Add a new throw cont using all of the old continuations
(define contextUpdate
  (lambda (ctx finally)
    (ctx_ret
     (setCtx_break
      (setCtx_cont
       (setCtx_throw ctx (lambda (v) ((throw_ctx ctx) (finally v))))
       (lambda (v) ((cont_ctx ctx) (finally v))))
      (lambda (v) ((break_ctx ctx) (finally v))))
     (lambda (v) ((ret_ctx ctx) (finally v))))))

; ------------------------------------------------------------------------------------------------------------------------------
; These are our M_class function calls which interpret and return the class calls

(define parlist caddr)
(define bod cadddr)

; Add the context field to the class
(define M_class_staticDeclare
  (lambda (stmt env ctx)
    (let* ((class (class_ctx ctx)))
      (setClass_field class (add_env (class_field class) (cadr stmt)
                                     (if (= 3 (length stmt)) (interpret_value (parlist stmt) env ctx) 'undefined))) )))

; Returns the binding after class declaration
(define M_class_declare
  (lambda (stmt env ctx)
    (let* ((class (class_ctx ctx)))
      (setClass_instance class
       (addtolayer (instanceOfClass class) (cadr stmt)
                     (if (= 3 (length stmt)) (interpret_value (parlist stmt) env ctx) 'undefined))) )))


; Adds the function to the class and returns it
(define M_class_funcDeclare
  (lambda (funcDeclare env static ctx)
    (let* ((fname (cadr funcDeclare))
           (class (class_ctx ctx))
           (cname (class_name class)))
      (setClass_method class
       (add_env (class_method class) fname
                (list (parlist funcDeclare) (bod funcDeclare)
                      (lambda (env) 
                        (let ((class (check_bindState env cname)))
                          (ret_env_from_func cname env)))
                      (lambda (env)   
                        (check_bindState env cname))
                      (if static (lambda (v) 'null) (lambda (v) v))))) )))

; Executes each statement and returns the class
(define M_class
  (lambda (stmt env ctx)
    (cond
     ((null? stmt) (class_ctx ctx))
     ((eq? 'static-function (firststmt stmt)) (M_class_funcDeclare stmt env #t ctx))
     ((eq? 'function (firststmt stmt)) (M_class_funcDeclare stmt env #f ctx))
     ((eq? 'static-var (firststmt stmt)) (M_class_staticDeclare stmt env ctx))
     ((eq? 'var (firststmt stmt)) (M_class_declare stmt env ctx))
     ((list? stmt) (error "ERROR. Invalid statement in class declaration!"))
     (else (class_ctx ctx)) )))

; interprets each statement in class declaration
(define M_class_stmt
  (lambda (block env ctx)
    (cond
      ((null? block) (class_ctx ctx))
      (else (M_class_stmt (cdr block) env
                               (let ((newclass (M_class (car block) env ctx)))
                                 (setCtx_curClass (setCtx_class ctx newclass) newclass)) )) )))

; --------------------------------------------------------------------------------------------------
; The following are the new addition to the code using classes. This will help us lookup which class
; /instance to use.

; Function that looks up the variable given the box 
(define box_varSearch
  (lambda (var env class instance)
    (cond
     ((eqv? var 'this) (box instance))
     ((checkStateMember env var) (check_boxState env var))
     ((and (not (eq? 'null class)) (checklayers (class_field class) var)) (lookuplayer (class_field class) var))
     ((and (not (eq? 'null instance)) (checklayers (list (car (instanceOfClass class)) (instance_vals instance)) var))
      (lookuplayer (list (car (instanceOfClass class)) (instance_vals instance)) var))
     (else 'no_such_value))))

; Function to look up the value using the envionment 
(define searchFuncEnv
  (lambda (var env class instance)
    (cond
     ((checkStateMember env var) (check_bindState env var))
     ((checklayers (class_method class) var) (lookup_env (class_method class) var))
     (else (error "ERROR. Function not found!")))))

; Return the class contained by the variable 
(define searchVarClass
  (lambda (var env ctx)
    (cond
     ((not (list? var)) (error "ERROR. Wrong use of dot operator!"))
     ((eq? (object var) 'class) (list 'null var))
     ((eq? (object var) 'inst) (list var (class_instance var)))
     (else (error "ERROR. Wrong use of dot operator!")))))

; Converts the physical symbol and adds to a pair 
(define convertSymbol
  (lambda (leftside env class ctx)
    (if (list? leftside)
        (cond
         ((eq? (car leftside) 'funcall) (searchVarClass (M_value_funccall leftside env ctx) env ctx))
         ((eq? (car leftside) 'dot) (searchVarClass (unbox (searchDotVar leftside env ctx)) env ctx))
         ((eq? (car leftside) 'new) (searchVarClass (M_value_new leftside env ctx)  env ctx)))
        (let ((lookup (box_varSearch leftside env class (instClass_ctx ctx))))
          (cond
           ((eq? leftside 'this) (list (instClass_ctx ctx) (class_instance (instClass_ctx ctx))))
           ((eq? leftside 'super) (list (instClass_ctx ctx) (class_parent class)))
           ((eq? 'no_such_value lookup) (error "ERROR. Var not found!"))
           (else (searchVarClass (unbox lookup) env ctx)))) )))

; Return the dot function within the statement 
(define searchDotFunc
  (lambda (stmt env ctx)
    (let ((class_instance (convertSymbol (fname stmt) env (class_ctx ctx) ctx)))
      (cons (searchFuncEnv (expr stmt) (newstate) (fname class_instance) (object class_instance)) class_instance) )))

; Looks for the dot function given the statement 
(define searchFunc
  (lambda (stmt env ctx)
    (cond
      ((list? stmt) (searchDotFunc stmt env ctx))
      (else (list (searchFuncEnv stmt env (class_ctx ctx) (instClass_ctx ctx)) (instClass_ctx ctx) (class_ctx ctx))) )))

; Looks up the dot variable associated with the dot func 
(define searchDotVar
  (lambda (stmt env ctx)
    (let ((class_instance (convertSymbol (class_instance stmt) env (curClass_ctx ctx) ctx)))
      (box_varSearch (expr stmt) (newstate) (class_parent class_instance) (vartype class_instance)) )))

; Looks up the variable associated with the function 
(define searchVars
  (lambda (stmt env ctx)
    (cond
      ((list? stmt) (searchDotVar stmt env ctx))
      (else (box_varSearch stmt env (curClass_ctx ctx) (instClass_ctx ctx))) )))

; --------------------------------------------------------------------------------------------------
; The following are the new addition to the code using classes. This will help us determine which class
; our operations are used in. 

(define class_parent cadr)
(define class_name caddr)
(define class_field cadddr)
(define class_instance cadr)
(define instance_vals caddr)

(define newClass
  (lambda (parent name)
    (list 'class parent name
          (if (eq? parent 'null) (newlayer) (class_field parent))
          (if (eq? parent 'null) (newlayer) (class_method parent))
          (if (eq? parent 'null) (newlayer) (instanceOfClass parent)) )))

;; Creates a new instance of the class. 
(define newinstance
  (lambda (class)
    (list 'inst class '())))

(define class_method
 (lambda (l)
  (list-ref l 4)))

(define instanceOfClass 
	(lambda (l) 
		(list-ref l 5)))

(define setClass_field
  (lambda (class fields)
    (replaceVal class 3 fields)))

(define setClass_method
  (lambda (class methods)
    (replaceVal class 4 methods)))

(define setClass_instance
  (lambda (class inst)
    (replaceVal class 5 inst)))

;; Modifying an instance.
(define setInstant_vals
  (lambda (inst values)
    (list 'inst (class_instance inst) values)))


; ----------------------------------------------------------------------------------------------
; Tests

(interpret "part4tests/test1.javaish" 'A) 
(interpret "part4tests/test2.javaish" 'A) 
(interpret "part4tests/test3.javaish" 'A)     
(interpret "part4tests/test4.javaish" 'A)     
(interpret "part4tests/test5.javaish" 'A)     
(interpret "part4tests/test6.javaish" 'A)     
(interpret "part4tests/test7.javaish" 'C)  
(interpret "part4tests/test8.javaish" 'Square)
(interpret "part4tests/test9.javaish" 'Square)
(interpret "part4tests/test10.javaish" 'List)
(interpret "part4tests/test11.javaish" 'List)
(interpret "part4tests/test12.javaish" 'List)
(interpret "part4tests/test13.javaish" 'C) 