#lang racket
(require (for-syntax racket/base))
(require (for-syntax syntax/parse))
(require syntax/parse)
(require racket/syntax)
(require (for-syntax racket/syntax))
(provide (all-defined-out)) 

(define-syntax-rule (not-equal? u v)
  (not (equal? u v)))

(define-syntax kond
  (syntax-rules (qdlet)
    ((_) (raise-syntax-error 'kond "ya done messed up"))
    ((_ (q a) cl ...) (if q a (kond cl ...)))
    ((_ ((qdlet ((qq-exp a)) A)) cl ...)
     (match a
       [qq-exp A]
       [_ (kond cl ...)]))))

(define-syntax dlet
  (syntax-rules (: quasiquote)
    ((__ () A : mexp term)
     (match `term
       [`mexp A]))
    ((__ (cl . cls) A) (dlet (cl . cls) A : () ()))
    ((__ (((quasiquote e) t) . cls) A : mexp term)
     (dlet cls A : (e . mexp) (,t . term)))))



















