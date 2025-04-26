#lang racket

;macros:compiler directives
;when you run a racket program, the compiler reads through the file, applies any macros and verifies the code is syntatctically correct
;macros can be used to define new syntax
;take the syntax, turn the syntax into a list, manipulate the list, then turn the list back into syntax

;create a "debug" syntax that lets us debug some code
(define-syntax debug
  (lambda (syn)
    (define slist (syntax->list syn))
    (datum->syntax syn `(let ((save ,(cadr slist))) (begin (print save) (newline) save)))))

(define-syntax foreachdo
  (lambda (syn)
    (define slist (syntax->list syn))
    (datum->syntax syn `(map (lambda (,(cadr slist) ) ,(cadddr slist) ) ,(caddr slist) ))))