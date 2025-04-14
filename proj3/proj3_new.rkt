#lang racket
(require "functionParser.rkt")
; ===================================================================================================================================
; ===================================================================================================================================
;                                               author: Jessie, Maisoon 
;                                                Interpreter Project 3 
; ===================================================================================================================================
; ===================================================================================================================================


; to run the program, just do (run) in the terminal
; it sends the parsed input file and an empty state list, with a 0 return value
; and then it will print out the return value, which is the only value in the first list of the state
(define interpret
  (lambda ()
    (call-main (global-layer (parser "Input.rkt")))))

; global-layer: initiates global-layer with an empty state '(() ())
(define global-layer
  (lambda (stmts)
    (handle-statementlist stmts empty_state
                     (lambda (return) (error "Return is invalid"))
                     (lambda (next) next)
                     (lambda (break) (error "Break used outside of loop"))
                     (lambda (cont) (error "Continue used outside of loop"))
                     (lambda (ex val) (error "Uncaught exception thrown")))))

; Our secondary pass through the file, executing whatever is in the declared main() function.
; (If there is no main function, then get_func_closure will issue an error.)
(define call-main
  (lambda (global_state)
    ;call/cc might make this easier ughhhhhh
    (call/cc (lambda (ret) (handle-statementlist
                            (closure_body (get-function-closure 'main global_state))
                            (add-function-layer 'main global_state)
                            ret
                            (lambda (next) next)
                            (lambda (break) (error "Break used outside of loop"))
                            (lambda (cont) (error "Continue used outside of loop"))
                            (lambda (ex val) (error "Uncaught exception thrown.")))))))



; ------------------------------------------------------------------------------------------------------------------------------------
; ------------------------------------------------------------------------------------------------------------------------------------
;                                                     Mapping Functions
; ------------------------------------------------------------------------------------------------------------------------------------
; ------------------------------------------------------------------------------------------------------------------------------------

; M_state: takes in the parsed input and a state list and returns the state list
; inputs: tree - parse tree in nested list structure, state - '((var1 var2 var3) (val1 val2 val3))

(define M_state
  (lambda (stmt state return next break continue throw)
    (cond
      ((isReturn stmt)      (handle-return stmt state return throw))
      ((isDeclare stmt)     (handle-declare stmt state next throw))
      ((isAssign stmt)      (handle-assign stmt state next throw))
      ((isIf stmt)          (handle-if stmt state return next break continue throw))
      ((isWhile stmt)       (handle-while stmt state return next throw))
      ((isBlock stmt)       (handle-block stmt state return next break continue throw))
      ((isBreak stmt)       (break state))
      ((isContinue stmt)    (continue state))
      ((isTry stmt)         (handle-try stmt state return next break continue throw))
      ((isThrow stmt)       (throw (M_int (throw_block stmt) state throw) state))
      ((isFinally stmt)     (M_state (cdr stmt) (add-layer state) return next break continue throw))
      ((isFunction stmt)    (handle-funcdef stmt state next))
      ((isFuncCall stmt)    (eval-function stmt state next throw))
      (else                 error "Invalid statement: ~a" stmt))))

; M_int: takes an expression (can have subexpressions) and a state and returns a value -- pass on anything not related to ints to M_bool
(define M_int
  (lambda (stmt-list state throw)
    (M_int_helper stmt-list state (lambda (v) v) throw)))

(define M_int_helper
  (lambda (stmt-list state return throw)
    (cond
      ((number? stmt-list)                  (return stmt-list))
      ((eq? stmt-list 'true)                (return #t))
      ((eq? stmt-list 'false)               (return #f))
      ((not-list stmt-list)                    (return (get-variable stmt-list state)))
      ((eq? (car stmt-list) '+)          (M_int_helper (firstoperand stmt-list) state (lambda (v1) (M_int_helper (caddr stmt-list) state (lambda (v2) (return (+ v1 v2))) throw)) throw))
      ((and (eq? (car stmt-list) '-)     (not (null? (cddr stmt-list)))) (M_int_helper (firstoperand stmt-list) state
                                                                                (lambda (v1) (M_int_helper (caddr stmt-list) state
                                                                                                           (lambda (v2) (return (- v1 v2))) throw)) throw))                                     
      ((eq? (car stmt-list) '-)          (M_int_helper (firstoperand stmt-list) state
                                                     (lambda (v) (return (- 0 v))) throw))
      ((eq? (car stmt-list) '*)          (M_int_helper (firstoperand stmt-list) state
                                                     (lambda (v1) (M_int_helper (caddr stmt-list) state
                                                                                (lambda (v2) (return (* v1 v2))) throw)) throw))
      ((eq? (car stmt-list) '/)          (M_int_helper (firstoperand stmt-list) state
                                                     (lambda (v1) (M_int_helper (caddr stmt-list) state
                                                                                (lambda (v2) (return (quotient v1 v2))) throw)) throw))
      ((eq? (car stmt-list) '%)          (M_int_helper (firstoperand stmt-list) state
                                                     (lambda (v1) (M_int_helper (caddr stmt-list) state
                                                                                (lambda (v2) (return (remainder v1 v2))) throw)) throw))
      (else                              (M_bool stmt-list state return throw)))))

; M_bool: takes in a parse parse-tree, state and returns #t or #f
(define M_bool
  (lambda (stmt-list state return throw)
    (M_bool_helper stmt-list state return throw)))

(define M_bool_helper
  (lambda (stmt-list state return throw)
    (cond
      ((eq? 'true stmt-list)                 (return #t))
      ((eq? 'false stmt-list)                (return #f))
      ((not-list stmt-list)                  (return (get-variable stmt-list state)))
      ((isFuncCall stmt-list)                (return (eval-function stmt-list state return (lambda (v) v) throw)))
      ((eq? '! (operator stmt-list))         (M_bool_helper (firstoperand stmt-list) state
                                                            (lambda (v) (return (not v))) throw))
      ((eq? '&& (operator stmt-list))        (M_bool_helper (firstoperand stmt-list) state
                                                            (lambda (v1) (M_bool_helper (caddr stmt-list) state
                                                                                        (lambda (v2) (return (and v1 v2))) throw)) throw))
      ((eq? '|| (operator stmt-list))        (M_bool_helper (firstoperand stmt-list) state
                                                            (lambda (v1) (M_bool_helper (caddr stmt-list) state
                                                                                        (lambda (v2) (return (or v1 v2))) throw)) throw))
      ((eq? '== (operator stmt-list))        (M_int_helper (firstoperand stmt-list) state
                                                           (lambda (v1) (M_int_helper (caddr stmt-list) state
                                                                                      (lambda (v2) (return (eq? v1 v2))) throw)) throw))
      ((eq? '!= (operator stmt-list))        (M_int_helper (firstoperand stmt-list) state
                                                           (lambda (v1) (M_int_helper (caddr stmt-list) state
                                                                                      (lambda (v2) (return (not (eq? v1 v2)))) throw)) throw))
      ((eq? '< (operator stmt-list))         (M_int_helper (firstoperand stmt-list) state
                                                           (lambda (v1) (M_int_helper (caddr stmt-list) state
                                                                                      (lambda (v2) (return (< v1 v2))) throw)) throw))
      ((eq? '<= (operator stmt-list))        (M_int_helper (firstoperand stmt-list) state
                                                           (lambda (v1) (M_int_helper (caddr stmt-list) state
                                                                                      (lambda (v2) (return (<= v1 v2))) throw)) throw))
      ((eq? '> (operator stmt-list))         (M_int_helper (firstoperand stmt-list) state
                                                           (lambda (v1) (M_int_helper (caddr stmt-list) state
                                                                                      (lambda (v2) (return (> v1 v2))) throw)) throw))
      ((eq? '>= (operator stmt-list))        (M_int_helper (firstoperand stmt-list) state
                                                           (lambda (v1) (M_int_helper (caddr stmt-list) state
                                                                                      (lambda (v2) (return (>= v1 v2))) throw)) throw))
      (else                                  error "Bad operator: ~a" stmt-list))))

; ---------------------------------------------------------------------------------------------------------------------------------------------
;                                                       Handlers for statement types
; ---------------------------------------------------------------------------------------------------------------------------------------------
; handle-statementlist: handles statement lists -- i think this should help with using next instead of using end like in proj2???
(define handle-statementlist
  (lambda (stmt state return next break continue throw)
    (if (null? stmt)
        (next state)
        (M_state (car stmt) state return
                 (lambda (v) (handle-statementlist (cdr stmt) v return next break continue throw)) break continue throw))))

(define handle-declare
  (lambda (stmt state next throw)
    (if (null? (cddr stmt))
        (next (add-variable (second stmt) '() state))
        (next (add-variable (second stmt) (M_int (caddr stmt) state throw) state)))))

(define handle-return
  (lambda (stmt state return throw)
    (let ((x (M_int (cadr stmt) state throw)))
      (cond
        ((number? x) (return x))
        (x (return 'true))
        (else (return 'false))))))

(define handle-assign
  (lambda (stmt state next throw)
    (next (create-binding! (cadr stmt) (M_int (caddr stmt) state throw) state))))

(define handle-if
  (lambda (stmt state return next break continue throw)
    (if (M_bool (cadr stmt) state (lambda (v) v) throw)
        (M_state (caddr stmt) state return next break continue throw)
        (if (null? (cdddr stmt))
            (next state)
            (M_state (cadddr stmt) state return next break continue throw)))))

(define handle-while
  (lambda (stmt state return next throw)
    (recurse-while (cadr stmt) (caddr stmt) state return next throw)))

(define recurse-while
  (lambda (condition body state return next throw)
    (if (M_bool condition state (lambda (v1) v1) throw) (M_state body state return
                 (lambda (v2) (recurse-while condition body v2 return next throw))
                 (lambda (v3) (next v3))                                 
                 (lambda (v4) (recurse-while body v4 return next throw)) 
                 throw)
        (next state))))

(define handle-block
  (lambda (stmts state return next break continue throw)
    (handle-statementlist (cdr stmts) (add-layer state) return
                          (lambda (v1) (next (pop-layer v1)))
                          (lambda (v2) (break (pop-layer v2)))
                          (lambda (v3) (continue (pop-layer v3)))
                          (lambda (v4 v5) (throw v4 (pop-layer v5))))))

(define handle-try
  (lambda (stmt state return next break continue throw)
    (let* ((try_stmts (make-try-block (try_block stmt)))
           (finally_stmts (make-finally-block (finally_block stmt)))
           (new_return (lambda (v) (handle-block finally_stmts state return (lambda (s) (return s)) break continue throw)))
           (new_break (lambda (v) (handle-block finally_stmts state return (lambda (s) (break s)) break continue throw)))
           (new_continue (lambda (v) (handle-block finally_stmts state return (lambda (s) (continue s)) break continue throw)))
           (new_throw (create_throw_continuations (catch_block stmt) state return next break continue throw finally_stmts)))
      (handle-block try_stmts state new_return (lambda (st) (handle-block finally_stmts st return next break continue throw)) new_break new_continue new_throw))))
    
(define make-try-block
  (lambda (try-statement)
    (cons 'begin try-statement)))

(define make-finally-block
  (lambda (finally-statement)
    (cond
      ((null? finally-statement) '(begin))
      ((not (eq? (car finally-statement) 'finally)) (error "Incorrectly formatted finally block"))
      (else (cons 'begin (cadr finally-statement))))))

(define create_throw_continuations
  (lambda (stmt state return next break continue throw finally)
    (cond
      ((null? stmt) (lambda (ex st) (handle-block finally state return (lambda (st) (throw ex st)) break continue throw)))
      ((not (eq? (car stmt) 'catch)) (error "Incorrect catch statement."))
      (else (lambda (ex st) (handle-statementlist
                             (caddr stmt)
                             (add-variable (catch_var stmt) ex (add-layer state))
                             return
                             (lambda (st1) (handle-block finally (pop-layer st1) return next break continue throw))
                             (lambda (st1) (break (pop-layer st1)))
                             (lambda (st1) (continue (pop-layer st1)))
                             (lambda (ex1 st1) (throw ex1 (pop-layer st1)))))))))

; -------------------------------------------------------- Function Implementation functions ----------------------------------------------------------------------

; handle-funcdef: creates function definition with name and parameters
(define handle-funcdef
  (lambda (stmt state next)
    (next (add-function (function-name stmt) (function-params stmt) (function-body stmt) state))))


;this is hopefully gonna evaluate a function and return a value?
; eval-function: evaluates a given function called in stmt-list
(define eval-function
  (lambda (stmt state return next throw)
    (let ((closure (get-function-closure (function-name stmt) state)))
      (cond
        ; parameter count mismatch
        [(not (= (num-params (closure_params closure)) (num-params (params stmt)))) (error "Parameter mismatch (expected ~a, got ~a)" (num-params (closure_params closure)) (num-params (params stmt)))]
        ; otherwise evaluate the function body
        (else (handle-statementlist
        (closure_body closure)
        (bind-params (closure_params closure)
                     (params stmt)
                     state
                     (add-function-layer (function-name stmt)
                                         ((closure_getstate closure) state))
                     throw)
        return
        next
        (lambda (v1) (error "Break outside of loop"))
        (lambda (v2) (error "Continue outside of loop"))
        (lambda (v3 v4) (throw v3 state))))))))

; helpers for function evaluation:

; bind-params:binding the formal params to actual params
; pass-by-reference
(define bind-params
  (lambda (formal-params actual-params state function-state throw)
    (cond
      ((null? formal-params) function-state)
      ((eq? (car formal-params) '&) (if (not (not-list (car actual-params)))
                                               (error "Variable name expected, ~a received" (car actual-params))
                                               (bind-params (cddr formal-params)
                                                            (cdr actual-params) state
                                                            (add-ptr (cadr formal-params) (get-ptr (car actual-params) state) function-state) throw)))
      (else bind-params (cdr formal-params) (cdr actual-params) state
                         (add-variable (car formal-params) (M_int (car actual-params) state throw) function-state) throw))))

; add-function: creates a function binding
(define add-function
  (lambda (name params body state)
    (cond
      ((check-binding name state)  (error "Function name already declared: ~a" name))
      ((null? (cddr state))    (list (cons name (car state)) (cons (create-closure params body state) (cadr state))))
      (else                    (append (list (cons name (car state)) (cons (create-closure params body state) (cadr state))) (pop-layer state))))))

; get-function-closure: retrieve function closure
(define get-function-closure
  (lambda (name state)
    (cond
      ((equal? state empty_state) (error "Function not found: ~a" name))
      ((not-list (car state)) (get-function-closure name (cdr state)))
      ((null? (car state)) (get-function-closure name (pop-layer state)))
      ((and (eq?  (caar state) name) (list? (car (cadr state)))) (caadr state))
      (else (get-function-closure name (cons (cdr (car state)) (cons (cdr (cadr state)) (cddr state))))))))

; A function to count the number of parameters.
(define num-params
  (lambda (param_list)
    (cond
      ((null? param_list) 0)
      ((eq? (car param_list) '&) (num-params (cdr param_list)))
      (else (+ 1 (num-params (cdr param_list)))))))

; ------------------------------------------------------------------------------------------------------------------------------------
; ------------------------------------------------------------------------------------------------------------------------------------
;                                                   Binding Functions
; ------------------------------------------------------------------------------------------------------------------------------------
; ------------------------------------------------------------------------------------------------------------------------------------

; get-var: looks up the current value of a variable in the environment.
(define get-variable
  (lambda (var state)
    (displayln "DEBUG in get-variable:")  
    (displayln state)       
    (cond
      [(equal? state empty_state) (error 'varerror "Variable not declared: ~a" var)]
      [(not-list (car state)) (get-variable var (cdr state))]
      [(null? (car state)) (get-variable var (pop-layer state))]
      [(eq? var (car (car state)))
       (cond
         {(not (box? (car (cadr state)))) (get-variable var (cdr state))}
         {(void? (unbox (car (cadr state)))) (error 'varerror "Variable not assigned: ~a" var)}
         {else (unbox (car (cadr state)))})]
      [else (get-variable var (cons (cdr (car state)) (cons (cdr (cadr state)) (cddr state))))])))


;get-ptr: looks up a pointer (box) to the storage of a var in the env (pass-by-ref)
(define get-ptr
  (lambda (var state)
    (cond
      ((equal? state empty_state) (error 'varerror "Variable not declared: ~a" var))
      ((not-list (car state)) (get-ptr var (cdr state)))
      ((null? (car state)) (get-ptr var (pop-layer state)))
      ((and (eq? var (car (car state))) (box? (car (cadr state)))) (car (cadr state)))
      (else (get-ptr var (cons (cdr (car state)) (cons (cdr (cadr state)) (cddr state))))))))

; adds var to state
(define add-variable
  (lambda (var val state)
    (cond
      ((check-binding var state) (error "Variable already declared: ~a" var))
      ((null? (cddr state)) (list (cons var (car state)) (cons (box val) (car state))))
      (else (append (list (cons var (car state)) (cons (box val) (cadr state))) (pop-layer state))))))

(define add-ptr
  (lambda (var ptr state)
    (cond
      ((check-binding var state) (error "Variable already declared: ~a" var)0)
      ((null? (cddr state)) (list (cons var (car state)) (cons ptr (cadr state))))
      (else (append (list (cons var (car state)) (cons ptr (cadr state))) (pop-layer state))))))

;check-binding takes in var name, state and returns true if var already exists, false otherwise
(define check-binding
  (lambda (var state)
    (cond
      ((not-list (car state)) (eq? (car state) var))
      ((null? (car state)) (if (null? (cddr state)) #f (check-binding var (cddr state))))
      ((eq? var (car (car state))) #t)
      (else (check-binding var (append (list (cdr (car state)) (cdr (cadr state))) (cddr state)))))))

; create-binding!: assigns a value to a variable.
; uses set-box!
(define create-binding!
  (lambda (var value state)
    (call/cc (lambda (end) (create-binding-helper! var value state state)))))

(define create-binding-helper!
  (lambda (var value state end)
    (cond
      [(equal? state empty_state) (error 'varerror "Variable not declared: ~a" var)]
      [(not-list (car state)) (create-binding-helper! var value (cdr state) end)] 
      [(null? (car state)) (create-binding-helper! var value (pop-layer state) end)]
      [(eq? var (car (car state))) (begin (set-box! (car (cadr state)) value) end)]
      [else (create-binding-helper! var value (cons (cdr (car state)) (cons (cdr (cadr state)) (cddr state))) end)])))

; add-layer: adds a new block layer for the state.
(define add-layer
  (lambda (state)
    (append empty_state state)))

; add-function-layer: adds a new function layer for the state, and append the function name
(define add-function-layer
  (lambda (name state)
    (append empty_state (list (list name)) state)))

; pop-layer the outermost layer from state
(define pop-layer
  (lambda (state)
    (if (or (null? (cddr state)) (not-list (car state)))
        (error 'stateerror "Invalid state")
        (cddr state))))



;this returns a tuple containing a list of parameters, a list containing the function body, then an integer n, where the n deepest layers are the ones that are in scope of the function
(define create-closure
  (lambda (params body state)
    (list params body (lambda (v) (find-state state v)))))

;helper: consider variables and functions on the same lexical layers to be in scope
(define find-state
  (lambda (orig_state given_state)
    (take-right given_state (length orig_state))))

  

; ------------------------------------------------------------------------------------------------------------------------------------
; ------------------------------------------------------------------------------------------------------------------------------------
;                                                   Abstraction Functions
; ------------------------------------------------------------------------------------------------------------------------------------
; ------------------------------------------------------------------------------------------------------------------------------------

(define isDeclare
  (lambda (lis)
    (eq? 'var (car lis))))

(define isAssign
  (lambda (lis)
    (eq? '= (car lis))))

(define isReturn
  (lambda (lis)
    (eq? 'return (car lis))))

(define isIf
  (lambda (lis)
    (eq? 'if (car lis))))

(define isWhile
  (lambda (lis)
    (eq? 'while (car lis))))

(define isBlock
  (lambda (stmt)
    (and (pair? stmt) (eq? 'begin (car stmt)))))

(define isBreak
  (lambda (stmt)
    (and (pair? stmt) (eq? 'break (car stmt)))))

(define isContinue
  (lambda (stmt)
    (and (pair? stmt) (eq? 'continue (car stmt)))))

(define isThrow
  (lambda (stmt)
    (and (pair? stmt) (eq? 'throw (car stmt)))))

(define isTry
  (lambda (stmt)
    (and (pair? stmt) (eq? 'try (car stmt)))))

(define isCatch
  (lambda (stmt)
    (and (pair? stmt) (eq? 'catch (car stmt)))))

(define isFinally
  (lambda (stmt)
    (and (pair? stmt) (eq? 'finally (car stmt)))))

(define isFunction
  (lambda (stmt)
    (and (pair? stmt) (eq? 'function (car stmt)))))

(define isFuncCall
  (lambda (stmt)
    (and (pair? stmt) (eq? 'funcall (car stmt)))))

(define operator car)
(define firstoperand cadr)
(define secondoperand caddr)

(define not-list
  (lambda (x)
    (not (or (pair? x) (null? x)))))


; try-catch-finally abstractions:
(define finally_block cadddr)
(define catch_block caddr)
(define catch_var caadr)
(define try_block cadr)
(define throw_block cadr)

; function abstractions
(define function-name cadr)
(define function-params caddr)
(define function-body cadddr)
(define params cddr)
(define closure_params car)
(define closure_body cadr)
(define closure_getstate caddr)



(define empty_state '(()()))