#lang scheme
(require "simpleParser.rkt")

; ===================================================================================================================================
; ===================================================================================================================================
;                                               author: Jessie, Maisoon 
;                                                Interpreter Project 1 
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
(define M_state
  (lambda (tree state)
    (cond
      ((null? tree)        state)
      ((list? (car tree))  (M_state (cdr tree) (M_state (first tree) state)));call the main method on the cdr and pass in the state updated by the car
      ((isDeclare tree)    (if (null? (cddr tree));if its just var x instead of var x = 10
                               (create-binding (second tree) '() state)
                               (create-binding (second tree) (M_value (caddr tree) state) state)))
      ((isAssign tree)     (update-binding (cadr tree) (M_value (caddr tree) state) state))  ; shooting into the dark rn ;(list (list 0)));placeholder
      ((isReturn tree)     (update-binding 'return (if (number? (M_value (cadr tree) state)) 
                                               (M_value (cadr tree) state) 
                                               (if (M_bool (cadr tree) state) 'true 'false))
                                           state))
      ((isIf tree)         (if (M_bool (cadr tree) state)
                               (M_state (caddr tree) state)
                               (if (null? (cdddr tree))
                                   state
                                   (M_state (cadddr tree) state))))    ;TODO: check with Adam to see if else-if handling is needed 
      ((isWhile tree)      (if (M_bool (cadr tree) state)
                               (M_state tree (M_state (caddr tree) state))
                               state)))))


; M_value: takes the parse tree and state list and returns either a bool or int based on the value of the expression
;might be better to use expr than tree here, it makes more sense
(define M_value
  (lambda (tree state)
    (cond
      ((null? tree)                                       0)
      ((boolean? tree)                                    tree)
      ((number? tree)                                     tree)
      ((not (pair? tree))                                 (get-value tree state))
      ((null? (cdr tree))                                 (M_value (car tree) state))
      ((eq? '+ (operator tree))                           (+ (M_int (firstoperand tree) state) (M_int (secondoperand tree) state)))
      ((and (eq? '- (operator tree)) (null? (cddr tree))) (* -1 (M_int (firstoperand tree) state)))
      ((eq? '- (operator tree))                           (- (M_int (firstoperand tree) state) (M_int (secondoperand tree) state)))
      ((eq? '* (operator tree))                           (* (M_int (firstoperand tree) state) (M_int (secondoperand tree) state)))
      ((eq? '/ (operator tree))                           (quotient (M_int (firstoperand tree) state) (M_int (secondoperand tree) state)))
      ((eq? '% (operator tree))                           (remainder (M_int (firstoperand tree) state) (M_int (secondoperand tree) state)))
      ((eq? '&& (operator tree))                          (and (M_bool (firstoperand tree) state) (M_bool (secondoperand tree) state)))
      ((eq? '|| (operator tree))                          (or (M_bool (firstoperand tree) state) (M_bool (secondoperand tree) state)))
      ((eq? '! (operator tree))                           (not (M_bool (firstoperand tree) state)))
      ((eq? '> (operator tree))                           (> (M_int (firstoperand tree) state) (M_int (secondoperand tree) state)))
      ((eq? '< (operator tree))                           (< (M_int (firstoperand tree) state) (M_int (secondoperand tree) state)))
      ((eq? '== (operator tree))                          (= (M_int (firstoperand tree) state) (M_int (secondoperand tree) state)))
      ((eq? '>= (operator tree))                          (>= (M_int (firstoperand tree) state) (M_int (secondoperand tree) state)))
      ((eq? '<= (operator tree))                          (<= (M_int (firstoperand tree) state) (M_int (secondoperand tree) state)))
      ((eq? '!= (operator tree))                          (not (= (M_int (firstoperand tree) state) (M_int (secondoperand tree) state))))
      (else                                               (error "value could not be understood")))));this just feels messy and inefficient

;M-int: takes an expression (can have subexpressions) and a state and returns a value
(define M_int
  (lambda (tree state)
    (cond
      ((null? tree)                                       0)
      ((number? tree)                                     tree)
      ((not (pair? tree))                                 (get-value tree state));when atom/var name passed in
      ((null? (cdr tree))                                 (M_int (car tree) state))
      ((eq? '+ (operator tree))                           (+ (M_int (firstoperand tree) state) (M_int (secondoperand tree) state)))
      ((and (eq? '- (operator tree)) (null? (cddr tree))) (* -1 (M_int (firstoperand tree) state)))
      ((eq? '- (operator tree))                           (- (M_int (firstoperand tree) state) (M_int (secondoperand tree) state)))
      ((eq? '* (operator tree))                           (* (M_int (firstoperand tree) state) (M_int (secondoperand tree) state)))
      ((eq? '/ (operator tree))                           (quotient (M_int (firstoperand tree) state) (M_int (secondoperand tree) state)))
      ((eq? '% (operator tree))                           (remainder (M_int (firstoperand tree) state) (M_int (secondoperand tree) state)))
      (else                                               (error 'bad-op "Invalid operator")))))

; M_bool: takes in a parse parse-tree, state and returns #t or #f
; TODO: map #t and #f to 'true' and 'false'
(define M_bool
  (lambda (tree state)
    (cond
      ((null? tree)              #f)
      ((eq? 'true tree)          #t)
      ((eq? 'false tree)         #f)
      ((boolean? tree)           tree)
      ((not (pair? tree))        (get-value tree state))
      ((null? (cdr tree))        (M_bool (car tree) state))
      ((eq? '&& (operator tree)) (and (M_bool (firstoperand tree) state) (M_bool (secondoperand tree) state)))
      ((eq? '|| (operator tree)) (or (M_bool (firstoperand tree) state) (M_bool (secondoperand tree) state)))
      ((eq? '! (operator tree))  (not (M_bool (firstoperand tree) state)))
      ((eq? '> (operator tree))  (> (M_int (firstoperand tree) state) (M_int (secondoperand tree) state)))
      ((eq? '< (operator tree))  (< (M_int (firstoperand tree) state) (M_int (secondoperand tree) state)))
      ((eq? '== (operator tree)) (= (M_int (firstoperand tree) state) (M_int (secondoperand tree) state)))
      ((eq? '>= (operator tree)) (>= (M_int (firstoperand tree) state) (M_int (secondoperand tree) state)))
      ((eq? '<= (operator tree)) (<= (M_int (firstoperand tree) state) (M_int (secondoperand tree) state)))
      ((eq? '!= (operator tree)) (not (= (M_int (firstoperand tree) state) (M_int (secondoperand tree) state))))
      (else (error 'bad-op "Invalid operator")))))

; ------------------------------------------------------------------------------------------------------------------------------------
; ------------------------------------------------------------------------------------------------------------------------------------
;                                                   Binding Functions
; ------------------------------------------------------------------------------------------------------------------------------------
; ------------------------------------------------------------------------------------------------------------------------------------

; TODO: we need to add some abstractions here --
; (caar state): car of var-sublist, (caadr state): car of val-sublist

; get-value: takes variable name and state function and finds the assigned value of the variable
; this was not working for some reason and i tried figuring out why cuz it was working at the start
; after debugging it seems it's because we're reducing the list bc of the else line
; we could try using let and comparing the index of the sublist using index-of and list-ref but i'm not sure if we're allowed to use that :(
; if you figure out the issue lmk

; UPDATE: ok so adding the error line when car state becomes null automatically terminates the code because it has no clue wtaf is happening
; so we have to probs work around it by using 0 or find a different way to throw an error when the whole state list is empty not just the car 


(define get-value;given variable name and state function, find the assigned value of the variable
  (lambda (var state)
    ;(displayln (format "get-value called with var: ~a, state: ~a" var state))  ; debugging
    (cond
      ((number? var)                                      var)
      ((boolean? var)                                     var)
      ((null? (car state))                                (error "variable could not be found"))
      ((and (eq? var (caar state)) (null? (caadr state))) (error "variable was not initialized"))
      ((eq? var (caar state))                             (caadr state))
      (else                                               (get-value var (list (cdar state) (cdadr state)))))))
 

; create-binding: takes a variable name, value, and state function and will return a state that contains the binding
; its assumed that the variable name doesnt already exist, theres no check in place
; TODO: add check later if needed
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

(define operator car)

(define firstoperand cadr)

(define secondoperand caddr)

(define first car)
(define second cadr)
(define third caddr)
(define fourth cadddr)

; TODO: add abstraction for state sublists
; TODO: add abstraction for parse tree too maybe, it might help with figuring out different branches and nodes

