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
    (get-value 'return (M_state-cps (parser "Input.rkt") '((return) (0))))))

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





(define M_state-cps   ;; same issue here where list? check and isBlock are causing issues idk why tho
  (lambda (stmts state)
    (cond
      ((null? stmts)          state)
      ((list? (car stmts))    (M_state-cps (cdr stmts) (M_state-cps (first stmts) state)))  ;; where the problem occurs;i also tracked a problem to here
      
      ((isDeclare stmts)  (handle-declare stmts state))
      ((isAssign stmts)   (handle-assign stmts state))
      ((isReturn stmts)    (handle-return stmts state))
      ((isIf stmts)       (handle-if stmts state))
      ;((isBreak (car stmts))  (handle-break state k))
      ;((isContinue (car stmts)) (handle-continue state k))
      ;((isThrow (car stmts)) (handle-throw (car stmts) state k))
      ;((isTry (car stmts)) (handle-try (car stmts) state k))
      ((isBlock stmts) (handle-block stmts state (lambda (v) v)))
      (else state))))


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

(define handle-block   ;; rewritten to consider the whole thing as a block
   (lambda (stmts state return)
    (cond
      ((eq? (caar stmts) 'while) (handle-while (car stmts) state))
      ((eq? (caar stmts) 'if) (handle-if (car stmts) state))
      (else (M_state-cps (cdr stmts) (M_state-cps (car stmts) state)))))) ;; using this allows the handle-if to be accessed through handle-block but not through M_state??

#| (define handle-block
      (lambda (stmts state return)
       (pop-layer (M_state-cps (cdr stmts) (add-layer state)))))
 |#


      


    
#| (define handle-break
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
         (else (M_state-cps (cadr stmt) (add-layer state)))))))) |#




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

(define get-value
  (lambda (var state)
    (cond
      ((null? state) (error "variable not found"))
      ((null? (car state)) (get-value var (cddr state)))
      ((eq? var (caar state)) (if (eq? '() (caadr state))
                                  (error "variable not initialized")
                                  (caadr state)))
      (else (get-value var (cons (cdar state) (cons (cdadr state) (cddr state))))))))

 

; create-binding: takes a variable name, value, and state function and will return a state that contains the binding  -- checks for a variable already existing or not
; TODO: handles top layers:
(define create-binding
  (lambda (var value state)
    (cond
      ((check-binding var state) (error "variable already declared in this block"))
      (else (cons (cons var (var-sublist state)) (cons (cons value (val-sublist state)) (cddr state)))))))

; update-binding: takes in var name, value and state and returns updated state, does nothing if var name doesnt exist
;im pretty sure this works but it just gets discarded when it goes back to M_state??
#| (define update-binding-cps
     (lambda (var val state return)
       (cond
         ((null? state) (error "variable must be declared"))
         ((null? (car state)) (update-binding-cps var val (cddr state) (lambda (v) (return (cons '() (cons '() v))))))
         ((eq? var (caar state)) (return (cons (car state) (cons (cons val (cdadr state)) (cddr state)))))
         (else (update-binding-cps var val (cons (cdar state) (cons (cdadr state) (cddr state)))
                                   (lambda (v) (return (cons (cons (caar state) (car v)) (cons (cons (caadr state) (cadr v)) (cddr state))))))))))
     |#

(define var-in-top-layer?
  (lambda (var varlist)
    (cond
      [(null? varlist) #f]
      [(eq? var (car varlist)) #t]
      [else (var-in-top-layer? var (cdr varlist))])))

(define update-val
  (lambda (var val varlist vallist)
    (cond
      [(null? varlist) '()] ; shouldn't happen if var was found
      [(eq? var (car varlist)) (cons val (cdr vallist))]
      [else (cons (car vallist)
                  (update-val var val (cdr varlist) (cdr vallist)))])))


(define update-binding-cps
  (lambda (var val state return)
    (cond
      ((null? state) (error "var must be declare"))
      ((var-in-top-layer? var (car state)) (return (cons (car state) (cons (update-val var val (car state) (cadr state)) (cddr state)))))
      (else (update-binding-cps var val (cddr state) (lambda (v) (return (cons (car state) (cons (cadr state) v)))))))))


(define update-binding
  (lambda (var val state)
    (update-binding-cps var val state (lambda (v) v))))
    #|(cond
      ((null? state) (error "variable must be declared"))
      ((null? (car state)) (update-binding var val (cddr state)))
      ((eq? var (caar state)) (cons (car state) (cons (cons val (cdadr state)) (cddr state))))
      (else (update-binding var val (cons (cdar state) (cons (cdadr state) (cddr state))))))))|#
    

;check-binding takes in var name, state and returns true if var already exists, false otherwise
(define check-binding
  (lambda (var state)
    (cond
      ((null? (car state))    #f)
      ((eq? var (caar state)) #t)
      (else                   (check-binding var (list (cdar state) (cdadr state)))))))



; TODO: add functions -- add_layer and pop_layer

(define add-layer
  (lambda (state)
     (cons '() (cons '() state))))

(define pop-layer
  (lambda (state)
    state)) 


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
    (or (pair? stmt) (eq? 'continue (car stmt)))))

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

