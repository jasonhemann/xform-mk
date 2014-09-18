#lang racket
(require "../mini.rkt")
(require "lang.rkt")

(define (member?o x ls out)
  (conde
    ((== '() ls) (== #f out))
    ((fresh (a d)
       (== `(,a . ,d) ls)
       (conde
         ((== x a) (== #t out))
         ((=/= x a) (member?o x d out)))))))

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
       (dlet ((`,b (member? a s2))
              (`,res (union d s2)))
         (kond
           ((equal? b #t) res)
           ((equal? b #f) `(,a . ,res))))))))

(union `(a b c) `(d c e))

