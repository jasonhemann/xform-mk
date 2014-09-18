#lang racket
(require "lang.rkt")

(define (member? x ls)
  (kond
    ((equal? '() ls) #f)
    ((qdlet ((`(,a . ,d) ls))
       (kond
         ((equal? x a) #t)
         ((not-equal? x a) (member? x d)))))))

(define (union s1 s2)
  (kond
    ((equal? '() s1) s2)
    ((qdlet ((`(,a . ,d) s1))
       (dlet ((`,b (member? a s2)))
         (cond
           ((equal? b #t) (union d s2))
           (else (cons a (union d s2)))))))))

(union `(a b c) `(d c e))

