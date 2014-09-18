#lang racket
(require "lang.rkt")

(define (member? x ls)
  (cond
    ((null? ls) #f)
    ((equal? x (car ls)) #t)
    (else (member? x (cdr ls)))))

(define (union s1 s2)
  (kond
    ((equal? '() s1) s2)
    ((qdlet ((`(,a . ,d) s1))
       (cond
         ((member? a s2) (union d s2))
         (else (cons a (union d s2))))))))

(union `(a b c) `(d c e))
