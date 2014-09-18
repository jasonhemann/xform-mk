#lang racket
(require "mini.rkt")

(transform-to-mini 
  (letrec
      ((union
        (lambda (s1 s2)
          (kond
            ((equal? `() `,s1) `,s2)
            ((qdlet ((`(,a . ,d) `,s1))
                    (dlet ((`,b (member? `,a `,s2))
                           (`,res (union `,d `,s2)))
                          (kond
                           ((equal? `,b `#t) `,res)
                           ((equal? `,b `#f) `(,a . ,res)))))))))
       (member?
        (lambda (x ls)
          (kond
           ((equal? `() `,ls) `#f)
           ((qdlet ((`(,a . ,d) `,ls))
                   (kond
                    ((equal? `,x `,a) `#t)
                    ((not-equal? `,x `,a) (member? `,x `,d)))))))))
   (union `(a b c) `(d c e))))



