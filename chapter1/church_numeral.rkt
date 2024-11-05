#lang racket

(define zero
  (λ (f) (λ (x) x)))

(define succ
  (λ (n) (λ (f) (λ (x) (f ((n f) x))))))

(define to-number
  (λ (c) ((c add1) 0)))

(define one   (succ zero))
(define two   (succ one))
(define three (succ two))
(define four  (succ three))
(define five  (succ four))
(define six   (succ five))
(define seven (succ six))
(define eight (succ seven))
(define nine  (succ eight))

(define plus
  (λ (n m)
    ((n succ) m)))

(define plus-alt
  (λ (n m)
    (λ (f)
      (λ (x)
        ((n f) ((m f) x))))))

(define cons
  (λ (a b)
    (λ (p) (p a b))))

(define car
  (λ (p)
    (p (λ (a b) a))))

(define cdr
  (λ (p)
    (p (λ (a b) b))))


(define pred
  (λ (n)
    (cdr ((n (λ (p) (cons (succ (car p)) (car p))))
             (cons zero zero)))))

(define pred2
  (λ (n)
    (λ (f) (λ (x) (cdr ((n (λ (p) (cons (f (car p)) (car p))))
                        (cons x x)))))))

(define pred3
  (λ (n)
    (λ (f) (λ (x) (((n (λ (p) (λ (m) (m (p (λ (a b) (b a))) f))))
                    (λ (m) (m x (λ (v) v)))) (λ (a b) a))))))

(display (to-number (pred3 eight)))