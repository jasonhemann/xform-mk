(define (overlap p r)
  (conde
    ((== p '(x y z)) (== r 'alpha-list))
    ((fresh (a d)
       (== `(,a . ,d) p)
       (== r 'generic-pair)))))

(run* (q) (overlap q 'generic-pair))
(run* (q) (overlap '(x y z) q))
