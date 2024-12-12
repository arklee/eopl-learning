#lang eopl

(define fib
  (lambda (n)
    (fib/k n (lambda (val) val))))

(define fib/k
  (lambda (n cont)
    (if (< n 2)
        (cont 1)
        (fib/k (- n 1)
               (lambda (val1)
                 (fib/k (- n 2)
                        (lambda (val2)
                          (cont (+ val1 val2)))))))))

(define n 'uninitialized)
(define cont 'uninitialized)
(define val 'uninitialized)

(define fact
  (lambda (arg-n)
    (set! cont (end-cont))
    (set! n arg-n)
    (fact/k)))

(define fact/k
  (lambda ()
    (if (zero? n)
        (begin
          (set! val 1)
          (apply-cont))
        (begin
          (set! cont (fact1-cont n cont))
          (set! n (- n 1))
          (fact/k)))))

(define-datatype continuation continuation?
  (end-cont)
  (fact1-cont
   (n integer?)
   (cont continuation?)))

(define apply-cont
  (lambda ()
    (cases continuation cont
      (end-cont () val)
      (fact1-cont (saved-n saved-cont)
                  (set! cont saved-cont)
                  (set! n saved-n)
                  (set! val (* val n))
                  (apply-cont)))))

(display (fact 5))
