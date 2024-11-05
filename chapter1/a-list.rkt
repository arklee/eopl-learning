#lang eopl

(define empty-env '())

(define empty-env?
 (lambda (env)
   (null? env)))

(define apply-env
  (lambda (var env)
    (cond [(empty-env? env) #f]
          [(eqv? var (car (car env))) (cdr (car env))]
          [else (apply-env var (cdr env))])))

(define has-binding?
  (lambda (var env)
    (cond [(empty-env? env) #f]
          [(eqv? var (car (car env))) #t]
          [else (apply-env var (cdr env))])))

(define extend-env-help
  (lambda (var val env)
    (if (eqv? var (car (car env)))
        (cons (cons var val) (cdr env))
        (cons (car env) (extend-env-help var val (cdr env))))))

(define extend-env
  (lambda (var val env)
    (cond [(has-binding? var env) (extend-env-help var val env)]
          [else (cons (cons var val) env)])))

(define remove-binding
  (lambda (var env)
    (cond [(empty-env? env) '()]
          [(eqv? var (car (car env))) (remove-binding var (cdr env))]
          [else (cons (car env) (remove-binding var (cdr env)))])))

(define extend-env-new
  (lambda (var val env)
    (cons (cons var val) (remove-binding var env))))

(define extend-env*
  (lambda (varl vall env)
    (if (null? varl)
        env
        (extend-env* (cdr varl)
                     (cdr vall)
                     (extend-env-new (car varl) (car vall) env)))))