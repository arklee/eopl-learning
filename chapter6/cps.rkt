#lang racket

(define (list-ref lst index)
  (cond
    [(null? lst) (error "Index out of bounds")]
    [(= index 0) (car lst)]
    [else (list-ref (cdr lst) (- index 1))]))

(define exps
  `((lambda (x y) (p (+ 8 x) (q y)))
    (lambda (x y u v) (+ 1 (f (g x y) (+ u v))))
    (+ 1 (f (g x y) (+ u (h v))))
    (zero? (if a (p x) (p y)))
    (zero? (if (f a) (p x) (p y)))
    (let ((x (let ((y 8)) (p y)))) x)
    (let ((x (if a (p x) (p y)))) x)))

(define name
    (let ((n 0))
      (lambda ()
        (set! n (+ n 1))
        (string-append "v" (number->string n)))))

(define cps-transform
  (lambda (exp rand? k)
    (match exp
      [`(lambda ,(list* x) ,body) (list 'lambda-exp x body)]
      [`(let ((,var ,exp1)) ,body) (list 'let-exp var exp1 body)]
      [`(if ,test ,exp1 ,exp2) (list 'if-exp test exp1 exp2)]
      [`(zero? ,arg) (list 'zero?-exp arg)]
      [`(,op ,arg1 ,arg2)
       #:when (or (eq? op '+) (eq? op '-) (eq? op '*) (eq? op '/))
       (cps-transform arg1 #t
                      (lambda (v1)
                        (cps-transform arg2 #t
                                       (lambda (v2)
                                         (k `(,op ,v1 ,v2))))))]
      [(list-rest rator rand)
       (let ((vn (name)))
         (cps-transform rand #f
                        (lambda (v)
                          (append (list rator v)
                                  `((lambda (,vn)
                                      ,(k vn)))))))]
      [x (k x)]
      [_ '()])))

(display (cps-transform '(f x) #f (lambda (v) v)))

(define cps-exps
  '((lambda (x y cont)
      (q y (lambda (val)
             (p (+ 8 x) val))))
    (lambda (x y u v cont)
      (g x y (lambda (val)
               (f val (+ u v) (lambda (val)
                                (+ 1 val))))))
    (g x y (lambda (val1)
             (h v (lambda (val2)
                    (f val1 (+ u val2) (lambda (val3)
                                         (cont (+ 1 val3))))))))
    (let ((cont (lambda (val)
                  (zero? val))))
      (if a
          (p x cont)
          (p y cont)))
    (let ((cont (lambda (val)
                  (zero? val))))
      (f a (lambda (val)
             (if val
                 (p x cont)
                 (p y cont)))))
    (let ((y 8))
      (p y (lambda (val)
             (let ((x val))
               x))))
    ((let ((cont (lambda (val)
                   (let ((x val))
                     x))))
       (if a
           (p x cont)
           (p y cont))))))

(define test
  (lambda (exp1s exp2s)
    (let ((test1 (lambda (exp1 exp2)
                   (equal? (cps-transform exp1) exp2))))
      (letrec ((test-all
                (lambda (exp1s exp2s)
                  (if(null? exp1s)
                     '()
                     (cons (test1 (car exp1s) (car exp2s)) (test-all (cdr exp1s) (cdr exp2s)))))))
        (test-all exp1s exp2s)))))

; (display "\n")
; (display (test exps cps-exps))