#lang scheme
(require rackunit)
(require rackunit/text-ui)
(require "GroupProject1.rkt")

; M_bool tests:
(define M-bool-test
  (test-suite "testing M_bool"
    (check-equal? (M_bool 'true '()) #t "M_bool should return #t for 'true'")
    (check-equal? (M_bool 'false '()) #f "M_bool should return #f for 'false'")
    (check-equal? (M_bool '(> 5 2) '()) #t "M_bool should evaluate (> 5 2) as #t")
    (check-equal? (M_bool '(! false) '()) #t "M_bool should evaluate (! false) as #t")
    (check-equal? (M_bool '(&& true false) '()) #f "M_bool should evaluate (true && false)as false")))


(run-tests (test-suite "All Tests" M-bool-test))      
