#lang racket
(require "mini.rkt")
(provide (all-defined-out) (all-from-out "mini.rkt"))

(define (app l s)
  (cond
    ((null? l) s)
    (else (cons (car l) (app (cdr l) s)))))

(define (appo l s o)
  (conde
    ((== l '()) (== s o))
    ((fresh (a d)
       (== `(,a . ,d) l)
       (fresh (res)
         (== `(,a . ,res) o)
         (appo d s res))))))

