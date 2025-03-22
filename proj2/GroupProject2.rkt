#lang scheme
(require "simpleParser.rkt")
(provide (all-defined-out))
; ===================================================================================================================================
; ===================================================================================================================================
;                                               author: Jessie, Maisoon 
;                                                Interpreter Project 2 
; ===================================================================================================================================
; ===================================================================================================================================


; to run the program, just do (run) in the terminal
; it sends the parsed input file and an empty state list, with a 0 return value
; and then it will print out the return value, which is the only value in the first list of the state
; run function to test:

(define run
  (lambda ()
    (get-value 'return (M_state-cps (parser "Input.rkt") '((return) (0)) (lambda (v) v)))))

;in order to be able to see what the parser will output
(define parse
  (lambda ()
    (parser "Input.rkt")))

; ------------------------------------------------------------------------------------------------------------------------------------
; ------------------------------------------------------------------------------------------------------------------------------------
;                                                     Mapping Functions
; ------------------------------------------------------------------------------------------------------------------------------------
; ------------------------------------------------------------------------------------------------------------------------------------

; M_state: takes in the parsed input and a state list and returns the state list
; inputs: tree - parse tree in nested list structure, state - '((var1 var2 var3) (val1 val2 val3))
;                                                           - the first sublist contains variable names & the second sublist contains their corresponding values
; TODO: add isBreak, isContinue, isThrow? Connamacher mentioned not to touch the current isIf and isWhile right now but I will watch the echo360 video again
; add return to the parameter and add return to the beginning of each one? check if that works

; isBlock  -- call add_layer, process every statement, then call remove_layer
; isBreak
; isContinue
; isThrow


#| (define M_state
     (lambda (stmt-list state)
       (cond
         ((null? stmt-list)                state)
         ((list? (car stmt-list))          (M_state (cdr stmt-list) (M_state (first stmt-list) state)))
         ((isDeclare stmt-list)            (if (null? (cddr stmt-list))
                                               (create-binding (second stmt-list) '() state)
                                               (create-binding (second stmt-list) (M_int (caddr stmt-list) state) state)))
         ((isAssign stmt-list)             (update-binding (cadr stmt-list) (M_int (caddr stmt-list) state) state))  
         ((isReturn stmt-list)             (update-binding 'return (if (number? (M_int (cadr stmt-list) state))
                                                                       (M_int (cadr stmt-list) state)
                                                                       (if (M_bool (cadr stmt-list) state) 'true 'false))        ; maps #t/#f to true/false
                                                                       state))
         ((isIf stmt-list)                 (if (M_bool (cadr stmt-list) state)
                                               (M_state (caddr stmt-list) state)
                                               (if (null? (cdddr stmt-list))
                                                   state
                                                   (M_state (cadddr stmt-list) state))))    
         ((isWhile stmt-list)              (if (M_bool (cadr stmt-list) state)
                                               (M_state stmt-list (M_state (caddr stmt-list) state))
                                            state))))) |#

(define M_state-cps
  (lambda (stmts state k)
    (cond
      ((null? stmts) (k state))
      ((list? (car stmts))
       (M_state-cps (car stmts) state
         (lambda (result)
           (cond
             ((or (eq? result 'BREAK) (eq? result 'CONTINUE)
                  (and (pair? result) (eq? (car result) 'THROW)))
              result)
             (else (M_state-cps (cdr stmts) result k))))))
      ((isBreak (car stmts)) (handle-break state k))
      ((isContinue (car stmts)) (handle-continue state k))
      ((isThrow (car stmts)) (handle-throw (car stmts) state k))
      ((isTry (car stmts)) (handle-try (car stmts) state k))
      ((isBlock (car stmts)) (handle-block (cdr (car stmts)) state k))
      (else (k state)))))
; -----------------------------------------------
; Handlers for statement types
; -----------------------------------------------

(define handle-declare
  (lambda (stmt state)
    (if (null? (cddr stmt))
        (create-binding (second stmt) '() state)
        (create-binding (second stmt) (M_int (caddr stmt) state) state))))

(define handle-assign
  (lambda (stmt state)
    (update-binding (cadr stmt) (M_int (caddr stmt) state) state)))

(define handle-return
  (lambda (stmt state)
    (update-binding 'return
                    (if (number? (M_int (second stmt) state))
                        (M_int (second stmt) state)
                        (if (M_bool (second stmt) state) 'true 'false))
                    state)))
(define handle-if
  (lambda (stmt state)
    (if (M_bool (cadr stmt) state)
        (M_state-cps (caddr stmt) state)
        (if (null? (cdddr stmt))
            state
            (M_state-cps (cadddr stmt) state)))))

(define handle-while
  (lambda (stmt state)
    (if (M_bool (cadr stmt) state)
        (M_state-cps stmt (M_state-cps (caddr stmt) state))
        state)))

(define handle-block
  (lambda (stmts state return)
    (M_state-cps stmts (add-layer state) (lambda (v) (return (pop-layer v))))))

(define handle-break
  (lambda (state)
    'BREAK))

(define handle-continue
  (lambda (state)
    'CONTINUE))

(define handle-throw
  (lambda (stmt state)
    (cons 'THROW (cdr stmt))))

(define handle-try
  (lambda (stmt state)
    (cond
      ((null? stmt) state)
      (else
       (cond
         ((and (pair? (M_state-cps (cadr stmt) (add-layer state)))
               (eq? 'THROW (car (M_state-cps (cadr stmt) (add-layer state)))))
          (cond
            ((eq? 'BREAK (M_state-cps (caddr stmt) (add-layer state))) 'BREAK)
            ((eq? 'CONTINUE (M_state-cps (caddr stmt) (add-layer state))) 'CONTINUE)
            (else (M_state-cps (caddr stmt) (add-layer state)))))
         (else (M_state-cps (cadr stmt) (add-layer state))))))))




; M_block: takes an expression (can have subexpressions) and a state --
; what is it really doing? you are entering a block, adding a layer to the state when vars are declared in the block, checking for if and while loops in the block itself (nested?)
;                          then once you get out of the block, you pop the layer off the state!
; therefore: add funcions for adding a layer (add_layer) and popping a layer (pop_layer)

; 2 ways to leave M_blovk: complete execution or because there is a break there

; M_int: takes an expression (can have subexpressions) and a state and returns a value -- pass on anything not related to ints to M_bool
(define M_int
  (lambda (stmt-list state)
    (cond
      ((null? stmt-list)                                             0)
      ((number? stmt-list)                                           stmt-list)
      ((not (pair? stmt-list))                                       (get-value stmt-list state))                    ;when atom/var name passed in
      ((null? (cdr stmt-list))                                       (M_int (car stmt-list) state))
      ((eq? '+ (operator stmt-list))                                 (+ (M_int (firstoperand stmt-list) state) (M_int (secondoperand stmt-list) state)))
      ((and (eq? '- (operator stmt-list)) (null? (cddr stmt-list)))  (* -1 (M_int (firstoperand stmt-list) state)))
      ((eq? '- (operator stmt-list))                                 (- (M_int (firstoperand stmt-list) state) (M_int (secondoperand stmt-list) state)))
      ((eq? '* (operator stmt-list))                                 (* (M_int (firstoperand stmt-list) state) (M_int (secondoperand stmt-list) state)))
      ((eq? '/ (operator stmt-list))                                 (quotient (M_int (firstoperand stmt-list) state) (M_int (secondoperand stmt-list) state)))
      ((eq? '% (operator stmt-list))                                 (remainder (M_int (firstoperand stmt-list) state) (M_int (secondoperand stmt-list) state)))
      (else                                                          (M_bool stmt-list state)))))

; M_bool: takes in a parse parse-tree, state and returns #t or #f
(define M_bool
  (lambda (stmt-list state)
    (cond
      ((null? stmt-list)              #f)
      ((eq? 'true stmt-list)          #t)
      ((eq? 'false stmt-list)         #f)
      ((boolean? stmt-list)           stmt-list)
      ((not (pair? stmt-list))        (get-value stmt-list state))
      ((null? (cdr stmt-list))        (M_bool (car stmt-list) state))
      ((eq? '&& (operator stmt-list)) (and (M_bool (firstoperand stmt-list) state) (M_bool (secondoperand stmt-list) state)))
      ((eq? '|| (operator stmt-list)) (or (M_bool (firstoperand stmt-list) state) (M_bool (secondoperand stmt-list) state)))
      ((eq? '! (operator stmt-list))  (not (M_bool (firstoperand stmt-list) state)))
      ((eq? '> (operator stmt-list))  (> (M_int (firstoperand stmt-list) state) (M_int (secondoperand stmt-list) state)))
      ((eq? '< (operator stmt-list))  (< (M_int (firstoperand stmt-list) state) (M_int (secondoperand stmt-list) state)))
      ((eq? '== (operator stmt-list)) (= (M_int (firstoperand stmt-list) state) (M_int (secondoperand stmt-list) state)))
      ((eq? '>= (operator stmt-list)) (>= (M_int (firstoperand stmt-list) state) (M_int (secondoperand stmt-list) state)))
      ((eq? '<= (operator stmt-list)) (<= (M_int (firstoperand stmt-list) state) (M_int (secondoperand stmt-list) state)))
      ((eq? '!= (operator stmt-list)) (not (= (M_int (firstoperand stmt-list) state) (M_int (secondoperand stmt-list) state))))
      (else (error 'bad-op "Invalid operator")))))

; ------------------------------------------------------------------------------------------------------------------------------------
; ------------------------------------------------------------------------------------------------------------------------------------
;                                                   Binding Functions
; ------------------------------------------------------------------------------------------------------------------------------------
; ------------------------------------------------------------------------------------------------------------------------------------

#| ; get-value: takes variable name and state function and finds the assigned value of the variable
   (define get-value        ;given variable name and state function, find the assigned value of the variable
     (lambda (var state)
       (displayln (format "get-value called with var: ~a, state: ~a" var state))  ; debugging
       (cond
         ((number? var)                                      var)
         ((boolean? var)                                     var)
         ((null? (car state))                                (error "variable could not be found"))
         ((and (eq? var (caar state)) (null? (caadr state))) (error "variable was not initialized"))
         ((eq? var (caar state))                             (caadr state))
      (else                                               (get-value var (list (cdar state) (cdadr state))))))) |#

(define search-value
  (lambda (var var-sublist val-sublist)
    (cond
      ((null? var-sublist) (error "variable was not initialized"))
      ((and (eq? var (car var-sublist)) (null? (car val-sublist))) (error "variable was not assigned"))
      ((eq? var (car var-sublist)) (car val-sublist))
      (else (search-value var (cdr var-sublist) (cdr val-sublist))))))

(define get-value
  (lambda (var state)
    (displayln (format "get-value called with var: ~a, state: ~a" var state))
    (search-value var (var-sublist state) (val-sublist state))))

 

; create-binding: takes a variable name, value, and state function and will return a state that contains the binding  -- checks for a variable already existing or not
; TODO: handles top layers:
(define create-binding
  (lambda (var value state)
    (cond
      ((check-binding var state) (error "variable already declared in this block"))
      (else (cons (cons var (var-sublist state)) (cons (cons value (val-sublist state)) (cddr state)))))))
;; idk if this is correct or not gotta test this


; update-binding: takes in var name, value and state and returns updated state, does nothing if var name doesnt exist
; update-binding: takes in var name, value and state and returns updated state, does nothing if var name doesnt exist
(define update-binding
  (lambda (var value state)
    (cond
      ((null? (var-sublist state))              (error "variable must be declared"));variable could not be found in state
      ((eq? var (car (var-sublist state)))      (list (var-sublist state) (cons value (cdr (val-sublist state)))))
      (else                                     (list
                                                 (cons (car (var-sublist state)) (car (update-binding var value (list (cdr (var-sublist state)) (cdr (val-sublist state))))))
                                                 (cons (car (val-sublist state)) (cadr (update-binding var value (list (cdr (var-sublist state)) (cdr (val-sublist state)))))))))))
;check-binding takes in var name, state and returns true if var already exists, false otherwise
(define check-binding
  (lambda (var state)
    (cond
      ((null? (car state))    #f)
      ((eq? var (caar state)) #t)
      (else                   (check-binding var (list (cdar state) (cdadr state)))))))

; if var-sublist is empty then return false
; if var-sublist has the value then return true
; else recursively call the function

; if we want to think of layers:
; if var-sublist is empty then return false
; 



; TODO: add functions -- add_layer and pop_layer

(define add-layer
  (lambda (state return)
    (return (cons '(() ()) state))))

(define pop-layer
  (lambda (state return)
    (return (cdr state)))) 



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
    (and (pair? stmt)(eq? 'begin (car stmt)))))

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

(define operator car)
(define firstoperand cadr)
(define secondoperand caddr)

(define first car)
(define second cadr)
(define third caddr)
(define fourth cadddr)

(define var-sublist car)
(define val-sublist cadr)

; TODO: add abstraction for state sublists
; TODO: add abstraction for parse tree too maybe, it might help with figuring out different branches and nodes

