#lang eopl

(define-datatype environment environment?
  (empty-env)
  (extend-env
   (var identifier?)
   (val expval?)
   (env environment?))
  (extend-env-letrec
   (var identifier?)
   (proc-var identifier?)
   (proc-body expression?)
   (env environment?)))

(define apply-env
  (lambda (env search-var)
    (cases environment env
      (empty-env () (report-no-binding-found search-var))
      (extend-env
       (saved-var saved-val saved-env)
       (if (eqv? search-var saved-var)
           saved-val
           (apply-env saved-env search-var)))
      (extend-env-letrec
       (var proc-var proc-body saved-env)
       (if (eqv? search-var var)
           (proc-val (procedure proc-var proc-body env))
           (apply-env saved-env search-var)))
      (else (report-invalid-env env)))))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment: ~s" env)))

(define the-store 'uninitialized)

(define store-size 0)
(define ref-num 0)

(define get-store
  (lambda () the-store))
3
(define extend-store!
  (lambda ()
    (let ([new-store (make-vector (* 2 store-size))])
      (letrec
          ([f (lambda (n)
                (if (< n store-size)
                    (begin (vector-set! new-store n (vector-ref the-store n))
                           (f (+ n 1)))
                    23))])
        (f 0)
        (set! store-size (* store-size 2))
        (set! the-store new-store)))))

(define initialize-store!
  (lambda (n)
    (set! the-store (make-vector n))
    (set! store-size n)))

(define reference?
  (lambda (v)
    (integer? v)))

(define newref
  (lambda (val)
    (begin
      (if (= ref-num store-size)
          (extend-store!)
          23)
      (vector-set! the-store ref-num val)
      (set! ref-num (+ ref-num 1))
      (- ref-num 1))))

(define deref
  (lambda (ref)
    (if (> ref (- ref-num 1))
        (eopl:error 'deref "reference too large for ~s" ref)
        (vector-ref the-store ref))))

(define setref!
  (lambda (ref val)
    (if (> ref (- ref-num 1))
        (eopl:error 'setref! "reference too large for ~s" ref)
        (vector-set! the-store ref val))))

(define identifier? symbol?)

(define-datatype proc proc?
  (procedure
   (var identifier?)
   (body expression?)
   (env environment?)))

(define apply-procedure
  (lambda (p val)
    (cases proc p
      (procedure (var body env)
                 (value-of body (extend-env var val env)))
      (else (eopl:error "~s not a procedure" p)))))

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (pair-val
   (car expval?)
   (cdr expval?))
  (emptylist)
  (proc-val
   (proc proc?)))

(define report-expval-extractor-error
  (lambda (type val)
    (eopl:error 'expval "~s is not a ~s exp" val type)))

(define expval->num
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (else (report-expval-extractor-error 'num val)))))

(define expval->bool
  (lambda (val)
    (cases expval val
      (bool-val (bool) bool)
      (else (report-expval-extractor-error 'bool val)))))

(define expval->car
  (lambda (val)
    (cases expval val
      (pair-val (car cdr) car)
      (else (report-expval-extractor-error 'pair val)))))

(define expval->cdr
  (lambda (val)
    (cases expval val
      (pair-val (car cdr) cdr)
      (else (report-expval-extractor-error 'pair val)))))

(define expval->proc
  (lambda (val)
    (cases expval val
      (proc-val (proc) proc)
      (else (report-expval-extractor-error 'procedure val)))))

(define-datatype expression expression?
  (const-exp
   (num number?))
  (diff-exp
   (exp1 expression?)
   (exp2 expression?))
  (minus-exp
   (exp1 expression?))
  (add-exp
   (exp1 expression?)
   (exp2 expression?))
  (mult-exp
   (exp1 expression?)
   (exp2 expression?))
  (quot-exp
   (exp1 expression?)
   (exp2 expression?))
  (equal?-exp
   (exp1 expression?)
   (exp2 expression?))
  (greater?-exp
   (exp1 expression?)
   (exp2 expression?))
  (less?-exp
   (exp1 expression?)
   (exp2 expression?))
  (zero?-exp
   (exp1 expression?))
  (if-exp
   (exp1 expression?)
   (exp2 expression?)
   (exp3 expression?))
  (var-exp
   (var identifier?))
  (let-exp
   (var identifier?)
   (exp1 expression?)
   (body expression?))
  (cons-exp
   (exp1 expression?)
   (exp2 expression?))
  (car-exp
   (exp1 expression?))
  (cdr-exp
   (exp1 expression?))
  (emptylist-exp)
  (null?-exp
   (exp1 expression?))
  (proc-exp
   (var identifier?)
   (body expression?))
  (apply-exp
   (proc expression?)
   (val expression?))
  (letproc-exp
   (var identifier?)
   (proc-var identifier?)
   (proc-body expression?)
   (body expression?))
  (letrec-exp
   (var identifier?)
   (proc-var identifier?)
   (proc-body expression?)
   (body expression?)))

(define value-of
  (lambda (exp env)
    (cases expression exp
      (const-exp (num) (num-val num))
      (diff-exp
       (exp1 exp2)
       (let [(val1 (value-of exp1 env))
             (val2 (value-of exp2 env))]
         (num-val (- (expval->num val1) (expval->num val2)))))
      (minus-exp (exp1)
                 (num-val (- (expval->num (value-of exp1 env)))))
      (add-exp
       (exp1 exp2)
       (let [(val1 (value-of exp1 env))
             (val2 (value-of exp2 env))]
         (num-val (+ (expval->num val1) (expval->num val2)))))
      (mult-exp
       (exp1 exp2)
       (let [(val1 (value-of exp1 env))
             (val2 (value-of exp2 env))]
         (num-val (* (expval->num val1) (expval->num val2)))))
      (quot-exp
       (exp1 exp2)
       (let [(val1 (value-of exp1 env))
             (val2 (value-of exp2 env))]
         (num-val (quotient (expval->num val1) (expval->num val2)))))
      (equal?-exp
       (exp1 exp2)
       (let [(val1 (value-of exp1 env))
             (val2 (value-of exp2 env))]
         (bool-val (= (expval->num val1) (expval->num val2)))))
      (greater?-exp
       (exp1 exp2)
       (let [(val1 (value-of exp1 env))
             (val2 (value-of exp2 env))]
         (bool-val (> (expval->num val1) (expval->num val2)))))
      (less?-exp
       (exp1 exp2)
       (let [(val1 (value-of exp1 env))
             (val2 (value-of exp2 env))]
         (bool-val (< (expval->num val1) (expval->num val2)))))
      (zero?-exp (exp)
                 (bool-val (= 0 (expval->num (value-of exp env)))))
      (var-exp (var) (apply-env env var))
      (let-exp
       (var exp1 body)
       (let [(val1 (value-of exp1 env))]
         (value-of body (extend-env var val1 env))))
      (if-exp
       (exp1 exp2 exp3)
       (if (expval->bool (value-of exp1 env))
           (value-of exp2 env)
           (value-of exp3 env)))
      (cons-exp
       (exp1 exp2)
       (let [(val1 (value-of exp1 env))
             (val2 (value-of exp2 env))]
         (pair-val val1 val2)))
      (car-exp
       (exp1)
       (let [(pair1 (value-of exp1 env))]
         (expval->car pair1)))
      (cdr-exp
       (exp1)
       (let [(pair1 (value-of exp1 env))]
         (expval->cdr pair1)))
      (emptylist-exp () (emptylist))
      (null?-exp
       (exp1)
       (let [(lst1 (value-of exp1 env))]
         (cases expval lst1
           (emptylist () (bool-val #t))
           (else (bool-val #f)))))
      (proc-exp
       (var body)
       (proc-val (procedure var body env)))
      (letproc-exp
       (var proc-var proc-body body)
       (let [(proc1 (proc-val (procedure proc-var proc-body env)))]
         (value-of body (extend-env var proc1 env))))
      (letrec-exp
       (var proc-var proc-body body)
       (value-of body (extend-env-letrec var proc-var proc-body env)))
      (apply-exp
       (exp1 exp2)
       (let [(proc1 (expval->proc (value-of exp1 env)))
             (val1 (value-of exp2 env))]
         (apply-procedure proc1 val1))))))

(define program-fact-rec
  (value-of
   (letrec-exp 'f
               'x
               (if-exp (zero?-exp (var-exp 'x))
                       (const-exp 1)
                       (mult-exp (var-exp 'x) (apply-exp (var-exp 'f) (diff-exp (var-exp 'x) (const-exp 1)))))
               (apply-exp (var-exp 'f) (const-exp 5)))
   (empty-env)))

;(display program-fact-rec)
