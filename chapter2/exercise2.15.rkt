#lang eopl

(define var-exp
  (lambda (var)
    (list 'var-exp var)))

(define lambda-exp
  (lambda (var lc-exp)
    (list 'lambda-exp var lc-exp)))

(define app-exp
  (lambda (lc-exp1 lc-exp2)
    (list 'app-exp lc-exp1 lc-exp2)))

(define var-exp?
  (lambda (lc-exp)
    (eqv? (car lc-exp) 'var-exp)))

(define lambda-exp?
  (lambda (lc-exp)
    (eqv? (car lc-exp) 'lambda-exp)))

(define app-exp?
  (lambda (lc-exp)
    (eqv? (car lc-exp) 'app-exp)))

(define var-exp->var
  (lambda (lc-exp)
    (cadr lc-exp)))

(define lambda-exp->bound-var
  (lambda (lc-exp)
    (cadr lc-exp)))

(define lambda-exp->body
  (lambda (lc-exp)
    (caddr lc-exp)))

(define app-exp->rator
  (lambda (lc-exp)
    (cadr lc-exp)))

(define app-exp->rand
  (lambda (lc-exp)
    (caddr lc-exp)))

(define occurs-free?
  (lambda (search-var exp)
    (cond
      ((var-exp? exp) (eqv? search-var (var-exp->var exp)))
      ((lambda-exp? exp)
       (and
        (not (eqv? search-var (lambda-exp->bound-var exp)))
        (occurs-free? search-var (lambda-exp->body exp))))
      (else
       (or
        (occurs-free? search-var (app-exp->rator exp))
        (occurs-free? search-var (app-exp->rand exp)))))))














