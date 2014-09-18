#lang racket
(require (for-syntax racket/base))
(require (for-syntax "mini.rkt")) 
(require "mini.rkt") 
(require (for-syntax syntax/parse))
(require (for-syntax (only-in racket/pretty pretty-print)))
(require (for-syntax racket/syntax))
(require (for-syntax macro-debugger/expand))
(require macro-debugger/expand)
(require rackunit)
(require rackunit/text-ui)
(provide (all-defined-out) expand/hide) 

(define-for-syntax (rename-name stx)
  (syntax-parse stx
    [stxx (format-id #'stxx "~ao" (syntax-e #'stxx))]))

(define-for-syntax (proc-def name stx)
  (syntax-parse name
    [n
     (syntax-parse stx
       [((~literal lambda) (formal:identifier ...) body_lhs:expr)
        (with-syntax* ((o (generate-temporary #'o))
                       (procd-A (proc-A #'body_lhs #'o)))
          #'(n (lambda (formal ... o) procd-A)))])]))

(define-for-syntax (proc-body stx)
  (syntax-parse stx
    [A:expr
     (with-syntax* ((o (generate-temporary #'q))
                    (cl-a (proc-A #'A #'o)))
       #'(run 1 (o) cl-a))]))

(define-for-syntax (proc-A stx o)
  (syntax-parse o
    [oo
       (syntax-parse stx
         [((~literal dlet) ((qq-exp E) ...) A)
          (with-syntax (((fv ...) (unique-fresh-vars (syntax->list #'(qq-exp ...))))) 
            (with-syntax (((pcl ...) (map proc-E (syntax->list #'(E ...)) (syntax->list #'(qq-exp ...)))))
              (with-syntax ((pbody (proc-A #'A #'oo)))
                #'(fresh (fv ...) pcl ... pbody))))]
         [((~literal kond) cl ...)
          (with-syntax (((pcl ...) (map (proc-kond-cl #'oo) (syntax->list #'(cl ...)))))
            #'(conde pcl ...))]
         [stxx (proc-E #'stxx #'oo)])]))

(define-for-syntax ((proc-kond-cl o) stxcl)
  (syntax-parse o
    [oo
     (syntax-parse stxcl
       [(((~literal qdlet) ((qq-exp E)) A))
        (with-syntax (((fv ...) (get-unique-vars #'qq-exp #'())))
          (with-syntax ((cl1 (proc-E #'E #'qq-exp))
                        (cl2 (proc-A #'A #'oo)))
            #'((fresh (fv ...) cl1 cl2))))]
       [(Q A)
        (with-syntax ((clq (proc-Q #'Q))
                      (cla (proc-A #'A #'oo)))
          #'(clq cla))])]))

(define-for-syntax (proc-Q stx)
  (syntax-parse stx
    ;; symbolo and numbero for later
    ;; [((~literal symbol?) ((~literal quasiquote) exp)) 
    ;;  #'(symbolo `exp)]
    [((~literal equal?) ((~literal quasiquote) exp1) ((~literal quasiquote) exp2))
     #'(== `exp1 `exp2)]
    [((~literal not-equal?) ((~literal quasiquote) exp1) ((~literal quasiquote) exp2))
     #'(=/= `exp1 `exp2)]))

(define-for-syntax (unique-fresh-vars stx)
  (syntax-parse stx
    [(qq-exp ...)  ;; does this have to be syntax, --v--- or can it just be a list?
     (with-syntax (((x ...) (foldr get-unique-vars #'() (syntax->list #'(qq-exp ...)))))
       #'(x ...))]))

(define-for-syntax (get-unique-vars qq-exp xs)
  (syntax-parse xs
    [(x ...)
;;     (pretty-print (syntax->datum #'(x ...)))
;;     (printf "^- x ... v- qq-exp~n")
;;     (pretty-print (syntax->datum qq-exp))
     (syntax-parse qq-exp
       ;; [((~literal unquote) (~var y (bindings-seen #'(x ...))))
       ;;  #'(x ...)]
       [((~literal quasiquote) ((~literal unquote) y:identifier))
        #'(y x ...)]
       [((~literal quasiquote) ((~literal unquote) y))
        #'(x ...)]
       [((~literal quasiquote) (a . d))
        (with-syntax* (((z ...) (get-unique-vars #'(quasiquote a) #'(x ...)))
                       ((z ...) (get-unique-vars #'(quasiquote d) #'(z ...))))
          #'(z ...))]
       [((~literal quasiquote) y)
        #'(x ...)])]))

(define-for-syntax (proc-E stx o)
  (syntax-parse o
    [oo
      (syntax-parse stx
        [((~literal quasiquote) t) #'(== `t oo)]
        [(f a ...)
         (with-syntax ((newf (rename-name #'f)))
           #'(newf a ... oo))])]))

(pretty-print-current-style-table
  (pretty-print-extend-style-table
    (pretty-print-current-style-table)
    '(fresh)
    '(lambda)))

(define-syntax transform-to-mini
  (lambda (stx)
    (syntax-parse stx
      [(transform-to-mini ((~literal letrec) ((Fname Flam) ...) A))
       (with-syntax ((Fnames (map rename-name (syntax->list #'(Fname ...)))))
         (with-syntax (((procd-def ...) (map proc-def (syntax->list #'Fnames) (syntax->list #'(Flam ...))))
                       (procd-body (proc-body #'A)))
           #'(syntax->datum
              (expand/hide
               #'(letrec (procd-def ...) procd-body)
               (list #'conde #'fresh #'run #'letrec)))))])))

(define-syntax transform-to-micro
  (lambda (stx)
    (syntax-parse stx
      [(transform-to-mini ((~literal letrec) ((Fname Flam) ...) A))
       (with-syntax ((Fnames (map rename-name (syntax->list #'(Fname ...)))))
         (with-syntax (((procd-def ...) (map proc-def (syntax->list #'Fnames) (syntax->list #'(Flam ...))))
                       (procd-body (proc-body #'A)))
           #'(syntax->datum
              (expand/hide
               #'(letrec (procd-def ...) procd-body)
               (list #'quasiquote #'letrec)))))])))

(define-syntax transform+run
  (lambda (stx)
    (syntax-parse stx
      [(transform-to-mk ((~literal letrec) ((Fname Flam) ...) A))
       (with-syntax ((Fnames (map rename-name (syntax->list #'(Fname ...)))))
         (with-syntax (((procd-def ...) (map proc-def (syntax->list #'Fnames) (syntax->list #'(Flam ...))))
                       (procd-body (proc-body #'A)))
           #'(letrec (procd-def ...) procd-body)))])))

(current-print pretty-write)