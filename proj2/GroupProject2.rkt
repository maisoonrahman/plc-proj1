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
    (get-value 'return (M_state (parser "Input.rkt") '((return) (0))))))

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


(define M_state
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
                                            state)))))

; TODO: add M-block
;     : I'll come up with a placeholder here

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

; get-value: takes variable name and state function and finds the assigned value of the variable
(define get-value        ;given variable name and state function, find the assigned value of the variable
  (lambda (var state)
    ;(displayln (format "get-value called with var: ~a, state: ~a" var state))  ; debugging
    (cond
      ((number? var)                                      var)
      ((boolean? var)                                     var)
      ((null? (car state))                                (error "variable could not be found"))
      ((and (eq? var (caar state)) (null? (caadr state))) (error "variable was not initialized"))
      ((eq? var (caar state))                             (caadr state))
      (else                                               (get-value var (list (cdar state) (cdadr state)))))))
 

; create-binding: takes a variable name, value, and state function and will return a state that contains the binding  -- checks for a variable already existing or not
(define create-binding
  (lambda (var value state)
    (if (check-binding var state)
        (error "variable already exists")
        (list (cons var (car state)) (cons value (cadr state))))))


; update-binding: takes in var name, value and state and returns updated state, does nothing if var name doesnt exist
(define update-binding
  (lambda (var value state)
    (cond
      ((null? (car state))    (error "variable must be declared"));variable could not be found in state
      ((eq? var (caar state)) (list (car state) (cons value (cdadr state))))
      (else                   (update-binding var value (list (cdar state) (cdadr state)))))))

;check-binding takes in var name, state and returns true if var already exists, false otherwise
(define check-binding
  (lambda (var state)
    (cond
      ((null? (car state))    #f)
      ((eq? var (caar state)) #t)
      (else                   (check-binding var (list (cdar state) (cdadr state)))))))

; TODO: add functions -- add_layer and pop_layer

(define add_layer
  (lambda (state return)
    (return (cons '(() ()) state))))

(define pop_layer
  (lambda (state return)
    (return (cdr state))))  ; I think this is cdr state?? i might have to test this to see if that works



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
  (lambda (lis)
    (eq? 'begin (car lis))))

(define isBreak
  (lambda (lis)
    (eq? 'break (car lis))))

(define isContinue
  (lambda (lis)
    (eq? 'continue (car lis))))

(define isThrow
  (lambda (lis)
    (eq? 'throw (car lis))))

(define isTry
  (lambda (lis)
    (eq? 'try (car lis))))

(define isCatch
  (lambda (lis)
    (eq? 'catch (car lis))))

(define isFinally
  (lambda (lis)
    (eq? 'finally (car lis))))

(define operator car)
(define firstoperand cadr)
(define secondoperand caddr)

(define first car)
(define second cadr)
(define third caddr)
(define fourth cadddr)

; TODO: add abstraction for state sublists
; TODO: add abstraction for parse tree too maybe, it might help with figuring out different branches and nodes

