#lang eopl

(define report-no-binding-found
  (lambda (n)
    (eopl:error "no variable found in ~s" n)))

(define empty-env
  (lambda ()
    (lambda (search-var)
      (report-no-binding-found search-var))))

(define extend-env
  (lambda (saved-var saved-val saved-env)
    (lambda (search-var)
      (if (eqv? search-var saved-var)
          saved-val
          (apply-env saved-env search-var)))))

(define apply-env
  (lambda (env search-var)
    (env search-var)))

(define empty-stack
  (lambda ()
    (lambda (saved-val)
      (report-no-binding-found saved-val))))

(define push-to-stack
  (lambda (val stack)
    (lambda (action)
      (cond [(eqv? action 'pop) stack]
            [(eqv? action 'check) val]
            [else (eopl:error "no action")]))))

(define pop-from-stack
  (lambda (stack)
    (stack 'pop)))

(define check-top
  (lambda (stack)
    (stack 'check)))


      


