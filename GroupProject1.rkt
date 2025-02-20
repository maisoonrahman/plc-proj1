#lang racket
(require "simpleParser.rkt")
;state is a list that contains two lists where indices correspond to eachother

;to run to program, just do (run) in the terminal
;it just sends the parsed input file and an empty state list, with a 0 return value
;and then it will print out the return value, which is the only value in the first list of the state
(define run
  (lambda ()
    (get-value 'return (M_state (parser "Input.rkt") '((return) (0))))))

;this is where we actually do everything? hopefully this can work
;it takes in the parsed input and a state list, and returns the state list
;this should probably be called M_state? thats the right usage right?
(define M_state
  (lambda (tree state)
    (cond
      ((null? tree)       state)
      ((list? (car tree)) (M_state (cdr tree) (M_state (car tree) state)));call the main method on the cdr and pass in the state updated by the car
      ((isDeclare tree)   (create-binding (cadr tree) (caddr tree) state));placeholder
      ((isAssign tree)    (list (list 0)));placeholder
      ((isReturn tree)    (update-binding 'return (M_int (cdr tree) state) state))
      ((isIf tree)        (if (M_bool (cadr tree) state)
                              (M_state (caddr tree) state)
                              (M_state (cadddr tree) state)));cannot handle else if
      ((isWhile tree)     (list (list 0))))));placeholder

;can handle both ints and booleans? thats the goal here
;to be used for return, since that needs to handle both int and bool
;i have no idea how to do this :/
;unfinished
(define M_value
  (lambda (tree state)
    (cond
      ((null? tree)             0)
      ((boolean? tree)          tree)
      ((number? tree)           tree)
      ((not (pair? tree))       (get-value tree state))
      ((boolean? (car tree))    (car tree))
      ((number? (car tree))     (car tree))
      ((not (pair? (car tree))) (get-value (car tree) state)))));this just feels messy and inefficient

;takes in an expression(can have subexpressions) and a state and returns a value
(define M_int
  (lambda (tree state)
    (cond
      ((null? tree)             0)
      ((number? tree)           tree)
      ((not (pair? tree))       (get-value tree state));when atom/var name passed in
      ((number? (car tree))     (car tree))
      ((list? (car tree))       (M_int (car tree) state))
      ((null? (cdr tree))       (get-value (car tree) state));check if tree is a variable and returns value
      ((eq? '+ (operator tree)) (+ (M_int (firstoperand tree) state) (M_int (secondoperand tree) state)))
      ((eq? '- (operator tree)) (- (M_int (firstoperand tree) state) (M_int (secondoperand tree) state)))
      ((eq? '* (operator tree)) (* (M_int (firstoperand tree) state) (M_int (secondoperand tree) state)))
      ((eq? '/ (operator tree)) (quotient (M_int (firstoperand tree) state) (M_int (secondoperand tree) state)))
      ((eq? '% (operator tree)) (remainder (M_int (firstoperand tree) state) (M_int (secondoperand tree) state))))))

;takes in a parse tree, state and returns #t or #f
(define M_bool
  (lambda (tree state)
    (cond
      ((null? tree)              #f)
      ((boolean? tree)           tree)
      ((boolean? (car tree))     (car tree))
      ((list? (car tree))        (M_bool (car tree) state))
      ((eq? '&& (operator tree)) (and (M_bool (firstoperand tree) state) (M_bool (secondoperand tree) state)))
      ((eq? '|| (operator tree)) (or (M_bool (firstoperand tree) state) (M_bool (secondoperand tree) state)))
      ((eq? '! (operator tree))  (not (M_bool (firstoperand tree) state)))
      ((eq? '> (operator tree))  (> (M_int (firstoperand tree) state) (M_int (secondoperand tree) state)))
      ((eq? '< (operator tree))  (< (M_int (firstoperand tree) state) (M_int (secondoperand tree) state)))
      ((eq? '== (operator tree)) (= (M_int (firstoperand tree) state) (M_int (secondoperand tree) state)))
      ((eq? '>= (operator tree)) (>= (M_int (firstoperand tree) state) (M_int (secondoperand tree) state)))
      ((eq? '<= (operator tree)) (<= (M_int (firstoperand tree) state) (M_int (secondoperand tree) state)))
      ((eq? '!= (operator tree)) (not (= (M_int (firstoperand tree) state) (M_int (secondoperand tree) state)))))))

;state binding and lookup functions
(define get-value;given variable name and state function, find the assigned value of the variable
  (lambda (var state)
    (cond
      ((null? (car state)) 0);i want this one to throw an error, idk how
      ((number? var) var)
      ((eq? var (caar state)) (caadr state))
      (else (get-value var (list (cdar state) (cdadr state)))))))

;takes in variable name and state list, and will return a pair of (name value)
;unfinished
;(define get-binding)

;takes in variable name, and state function and will return a state that contains the binding
;its assumed that the variable name doesnt already exist, theres no check in place
(define create-binding
  (lambda (name value state)
    (list (cons name (car state)) (cons value (cadr state)))))

;takes in var name, value and state and returns updated state, does nothing if var name doesnt exist
(define update-binding
  (lambda (name value state)
    (cond
      ((null? (caar state)) state);variable could not be found in state
      ((eq? name (caar state)) (list (car state) (cons value (cdadr state))))
      (else (update-binding name value (list (cdar state) (cdadr state)))))))
    
;some abstraction functions
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
