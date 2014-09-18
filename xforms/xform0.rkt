#lang racket
(require "lang.rkt")

(define (union s1 s2)
  (cond
    ((null? s1) s2)
    ((member (car s1) s2) (union (cdr s1) s2))
    (else (cons (car s1) (union (cdr s1) s2)))))

(union `(a b c) `(d c e))
