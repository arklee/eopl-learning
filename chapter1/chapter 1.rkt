#lang eopl
(define nth-element
  (lambda (lst n)
    (if (null? lst)
        (report-list-too-short n)
        (if (zero? n)
            (car lst)
            (nth-element (cdr lst) (- n 1))))))

(define report-list-too-short
  (lambda (n)
    (eopl:error 'nth-element 
           "List too short by ~s elements.~%" (+ n 1))))

(define remove-first
  (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? (car los) s)
            (cdr los)
            (cons (car los) (remove-first s (cdr los)))))))

(define occurs-free?
  (lambda (var exp)
    (cond [(symbol? exp) (eqv? var exp)]
          [(eqv? (car exp) 'lambda)
           (and
               (not (eqv? var (car (car (cdr exp)))))
               (occurs-free? var (car (cdr (cdr exp)))))]
          [else (or (occurs-free? var (car exp))
                   (occurs-free? var (car (cdr exp))))])))

(define subst
  (lambda (new old slist)
    (if (null? slist)
        '()
        (cons (subst-in-sexp new old (car slist))
              (subst new old (cdr slist))))))

(define subst-in-sexp
  (lambda (new old sexp)
    (if (symbol? sexp)
        (if (eqv? old sexp) new sexp)
        (subst new old sexp))))

(define subst2
  (lambda (new old slist)
    (if (null? slist)
        '()
        (map (lambda (sexp) (subst-in-sexp new old sexp)) slist))))

(define number-elements-iter
  (lambda (lst n)
    (if (null? lst)
        '()
        (cons (list n (car lst)) (number-elements-iter (cdr lst) (+ n 1))))))

(define number-elements
  (lambda (lst)
    (number-elements-iter lst 0)))

(define number-elements-alt
  (lambda (lst)
    (if (null? lst)
        '()
        (g (list 0 (car lst)) (number-elements-alt (cdr lst))))))

(define g
  (lambda (pair lst)
    (cons pair
          (map (lambda (p) (cons (+ 1 (car p)) (cdr p))) lst))))











     