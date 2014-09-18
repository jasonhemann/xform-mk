#lang racket
(require "../mini.rkt")

(define (member?o x ls out)
  (conde
    ((== '() ls) (== #f out))
    ((fresh (a d)
       (== `(,a . ,d) ls)
       (conde
         ((== x a) (== #t out))
         ((=/= x a) (member?o x d out)))))))

(define (uniono s1 s2 out)
  (conde
    ((== '() s1) (== s2 out))
    ((fresh (a d)
       (== `(,a . ,d) s1)
       (fresh (b res)
         (conde
           ((== b #t) (== res out))
           ((== b #f) (== `(,a . ,res) out)))
         (member?o a s2 b)
         (uniono d s2 res))))))

(run 1 (q) (uniono `(a b c) `(d c e) q))

(run 4 (q) (fresh (s1 s2) (== q `(,s1 ,s2)) (uniono s1 s2 `(a b d c e))))


