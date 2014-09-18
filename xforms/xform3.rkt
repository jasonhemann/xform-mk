#lang racket
(require "lang.rkt")

(define (member? x ls)
  (kond
    ((equal? '() ls) #f)
    ((qdlet ((`(,a . ,d) ls))
       (cond
         ((equal? x a) #t)
         (else (member? x d)))))))

(define (union s1 s2)
  (kond
    ((equal? '() s1) s2)
    ((qdlet ((`(,a . ,d) s1))
       (cond
         ((member? a s2) (union d s2))
         (else (cons a (union d s2))))))))

(union `(a b c) `(d c e))

