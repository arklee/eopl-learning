#lang eopl

(define number->sequence
  (lambda (n) (cons n '(() ()))))

(define current-element
  (lambda (seq)
    (car (seq))))

(define move-to-left
  (lambda (seq)
    (list (caadr seq) (cdadr seq) (cons (car seq) (caddr seq)))))

(define move-to-right
  (lambda (seq)
    (list (caaddr seq) (cons (car seq) (cadr seq)) (cdaddr seq) )))

(define insert-to-left
  (lambda (n seq)
    (list (car seq) (cons n (cadr seq)) (caddr seq))))

(define insert-to-right
  (lambda (n seq)
    (list (car seq) (cadr seq) (cons n (caddr seq)))))

(define number->bintree