#lang eopl

(define number->bintree
  (lambda (n)
    (list n '() '() '())))

(define insert-to-left
  (lambda (n seq)
    (list (car seq) (list n (cadr seq) '() '()) (caddr seq) '())))

(define insert-to-right
  (lambda (n seq)
    (list (car seq) (cadr seq) (list n (caddr seq) '() '()) '())))

(define move-to-left
  (lambda (seq)
    (let [(cur (cadr seq))]
      (list (car cur) (cadr cur) (caddr cur) (list (car seq) (caddr seq) (cadddr seq) 'left)))))

(define move-to-right
  (lambda (seq)
    (let [(cur (caddr seq))]
      (list (car cur) (cadr cur) (caddr cur) (list (car seq) (cadr seq) (cadddr seq)'right)))))

(define at-root?
  (lambda (seq)
    (null? (cadddr seq))))

(define move-up
  (lambda (seq)
    (let [(cur (cadddr seq))]
      (cond [(null? cur) (eopl:error "no parent node")]
            [(eqv? (cadddr cur) 'left)
             (list (car cur) (list (car seq) (cadr seq) (caddr seq) '()) (cadr cur) (caddr cur))]
            [else (list (car cur) (cadr cur) (list (car seq) (cadr seq) (caddr seq)) (caddr cur) '())]))))
          
(define current-element
  (lambda (seq)
    (car seq)))

(define t1
  (insert-to-right 14 (insert-to-left 12 (number->bintree 13))))

(define t2
  (insert-to-left 15 t1))

(define at-leaf?
  (lambda (seq)
    (null? seq)))

