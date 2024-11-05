#lang eopl

(define zero (lambda () '()))

(define is-zero? (lambda (x) (null? x)))

(define successor
  (lambda (x N)
    (cond [(null? x) '(1)]
          [(= (car x) (- N 1)) (cons 0 (successor (cdr x) N))]
          [else (cons (+ 1 (car x)) (cdr x))])))

(define predecessor
  (lambda (x N)
    (if (null? (cdr x))
        (if (= 1 (car x))
            '()
            (cons (- 1 (car x)) '()))
        (if (= 0 (car x))
            (cons (- N 1) (predecessor (cdr x) N))
            (cons (- (car x) 1) (cdr x))))))


