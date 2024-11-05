#lang eopl

(define list-of
  (lambda (pred)
    (lambda (val)
      (or (null? val)
          (and (pair? val)
               (pred (car val))
               ((list-of pred) (cdr val)))))))

(define identifier? symbol?)

(define-datatype lc-exp lc-exp?
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-vars (list-of identifier?))
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand (list-of lc-exp?))))

(define lc-exp-parser
  (lambda (exp)
    (cond [(symbol? exp) (var-exp exp)]
          [(pair? exp)
           (if (eqv? (car exp) 'lambda)
               (lambda-exp (cadr exp) (lc-exp-parser (caddr exp)))
               (app-exp (lc-exp-parser (car exp)) (map lc-exp-parser (cdr exp))))]
           [else (eopl:error 'lc-exp-parser "Invalid expression: ~s" exp)])))

(define-datatype prefix-exp prefix-exp?
  (const-exp
   (num integer?))
  (diff-exp
   (operand1 prefix-exp?)
   (operand2 prefix-exp?)))

(define search
  (lambda (exp)
    (if (eqv? (car exp) '-)
        (prefix-parser-help exp)
        (cons (const-exp (car exp)) (cdr exp)))))

(define prefix-parser-help
  (lambda (exp)
    (let* [(first (search (cdr exp)))
           (second (search (cdr first)))]
      (cons (diff-exp (car first) (car second)) (cdr second)))))

(define prefix-parser
  (lambda (exp)
    (car (prefix-parser-help exp))))

(display (prefix-parser '(- - 3 2 - 4 - 12 7)))