#lang racket

; example:

; (lambda (x) x)
; -> (lambda (x k) (k x))

; (lambda (x) (x 2))
; -> (lambda (x k) (x 2 k))

; (lambda (x) (f (g 2)))
; -> (lambda (x k) (g 2 (lambda (v1) (f v1 k))))

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

(define final-k
  (lambda (exp)
    (match exp
      [(list-rest rator rand) exp]
      [`(lambda ,(list* x) ,body) exp]
      [_ (list 'k exp)])))

(define cps-transform
  (lambda (exp k)
    (match exp
      [`(lambda ,(list* x) ,body)
      (list 'lambda (append x '(k)) (cps-transform body k))]
      [`(let ((,var ,exp1)) ,body) (list 'let-exp var exp1 body)]
      [`(if ,test ,exp1 ,exp2) (list 'if-exp test exp1 exp2)]
      [`(zero? ,arg) 
       (cps-transform arg
                      (lambda (v)
                        (k `(zero? ,v))))]
      [`(,op ,arg1 ,arg2)
       #:when (or (eq? op '+) (eq? op '-) (eq? op '*) (eq? op '/))
       (cps-transform arg1
                      (lambda (v1)
                        (cps-transform arg2
                                       (lambda (v2)
                                         (k `(,op ,v1 ,v2))))))]
      [(list-rest rator rand)
       (cps-transform rator
                      (lambda (v1)
                        (letrec ((cps-rand (lambda (exp)
                                             (match exp
                                               [`(zero? ,arg) (k `(,v1 ,exp))]
                                               [`(,op ,arg1 ,arg2)
                                                #:when (or (eq? op '+) (eq? op '-) (eq? op '*) (eq? op '/))
                                                (k `(,v1 ,exp))]
                                               [(list-rest rator rand)
                                                (let ((vn (name)))
                                                  `(lambda (,vn) ,(k `(,v1 ,vn))))]
                                               [x (k `(,v1 ,exp))]
                                               [_ 'error]))))
                          (cps-transform (car rand) cps-rand))))]
      [x (k x)]
      [_ 'error])))

(define cpser
  (lambda (e) (cps-transform e final-k)))

(display (cpser '(+ 2 3)))

; current: only "call", "var", "op"

; (cps (f (g x)))
; call-cont: rator=f saved-k=k
; val = (g x)

; (cps (g x)) with (call-cont: rator=f saved-k=k)
; call-cont: rator=g saved-k=(call-cont: rator=f saved-k=k)
; val = x

; (cps x) with (call-cont: rator=g saved-k=(call-cont: rator=f saved-k=k))
; (apply (call-cont: rator=g saved-k=(call-cont: rator=f saved-k=k)) x)

; (apply-cont (call-cont: rator=f saved-k=k) (g x))

; final (g x (lambda (v1) (f v1 k)))



; (cps (f 2))
; call-cont: rator=f saved-k=k
; val = 2

; (cps 2) with (call-cont: rator=f saved-k=k)
; (apply (call-cont: rator=f saved-k=k) 2)
; (f 2 k)

; final (f 2 k)

(define cps-exps
  '((lambda (x y k)
      (q y (lambda (val)
             (p (+ 8 x) val))))
    (lambda (x y u v k)
      (g x y (lambda (val)
               (f val (+ u v) (lambda (val)
                                (+ 1 val))))))
    (g x y (lambda (val1)
             (h v (lambda (val2)
                    (f val1 (+ u val2) (lambda (val3)
                                         (k (+ 1 val3))))))))
    (let ((k (lambda (val)
                  (zero? val))))
      (if a
          (p x k)
          (p y k)))
    (let ((k (lambda (val)
                  (zero? val))))
      (f a (lambda (val)
             (if val
                 (p x k)
                 (p y k)))))
    (let ((y 8))
      (p y (lambda (val)
             (let ((x val))
               x))))
    ((let ((k (lambda (val)
                   (let ((x val))
                     x))))
       (if a
           (p x k)
           (p y k))))))

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