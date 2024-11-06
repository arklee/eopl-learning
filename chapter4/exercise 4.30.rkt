#lang eopl

(define identifier? symbol?)

(define-datatype environment environment?
  (empty-env)
  (extend-env
   (var identifier?)
   (val denval?)
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
      (else (report-invalid-env env)))))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment: ~s" env)))

(define empty-store
  (lambda () '()))

(define the-store 'uninitialized)

(define get-store
  (lambda () the-store))

(define initialize-store!
  (lambda ()
    (set! the-store (empty-store))))

(define reference?
  (lambda (v)
    (integer? v)))

(define newref
  (lambda (val)
    (let ((next-ref (length the-store)))
      (set! the-store (append the-store (list val))) next-ref)))

(define deref
  (lambda (ref)
    (list-ref the-store ref)))

(define setref!
    (lambda (ref val)
      (set! the-store
        (letrec
          ((setref-inner
            (lambda (store1 ref1)
              (cond
                ((null? store1)
                 (report-invalid-reference ref the-store))
                ((zero? ref1)
                 (cons val (cdr store1)))
                (else
                 (cons
                  (car store1)
                  (setref-inner
                   (cdr store1) (- ref1 1))))))))
          (setref-inner the-store ref)))))

(define report-invalid-reference
  (lambda (ref the-store)
    (eopl:error 'setref! "invalid reference of ~s in ~s" ref the-store)))

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (proc-val
   (var identifier?)
   (body expression?)
   (saved-env environment?))
  (array-val
   (array array?)))

(define denval? number?)

(define ref-val
  (lambda (x) x))

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

(define denval->ref
  (lambda (val)
    (if (denval? val)
        val
        (report-expval-extractor-error 'reference val))))

(define-datatype array array?
  (an-array
   (firstloc reference?)
   (len integer?)))

(define newarray
  (lambda (len initval)
    (letrec ([f
              (lambda (len)
                (if (= len 0)
                    27
                    (begin
                      (newref initval)
                      (f (- len 1)))))])
      (let ([firstloc (newref initval)])
        (begin (f (- len 1))
               (an-array firstloc len))))))

(define arrayref
  (lambda (arr ref)
    (cases array arr
      (an-array
       (first len)
       (if (< ref len)
           (deref (+ ref first))
           (eopl:error "index ~s out of range ~s" ref len)))
      (else (eopl:error "~s is not an array" 'arr)))))

(define arrayset
  (lambda (arr ref val)
    (cases array arr
      (an-array
       (first len)
       (if (< ref len)
           (setref! (+ first ref) val)
           (eopl:error "index ~s out of range ~s" ref len)))
      (else (eopl:error "~s is not an array" 'arr)))))

(define arraylength
  (lambda (arr)
    (cases array arr
      (an-array (first len) len))))

(define expval->array
  (lambda (val)
    (cases expval val
      (array-val (array) array)
      (else (report-expval-extractor-error 'array val)))))

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
  (letrec-exp
   (var identifier?)
   (proc-var identifier?)
   (proc-body expression?)
   (body expression?))
  (proc-exp
   (var identifier?)
   (body expression?))
  (apply-exp
   (proc expression?)
   (val expression?))
  (begin-exp
    (lst (list-of expression?)))
  (assign-exp
   (var identifier?)
   (exp expression?))
  (array-exp
   (len expression?)
   (val expression?))
  (arrayref-exp
   (arr expression?)
   (ref expression?))
  (arrayset-exp
   (arr expression?)
   (ref expression?)
   (val expression?))
  (arraylength-exp
   (arr expression?)))

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
      (var-exp
       (var)
       (let ([val (apply-env env var)])
         (deref (denval->ref val))))
      (if-exp
       (exp1 exp2 exp3)
       (if (expval->bool (value-of exp1 env))
           (value-of exp2 env)
           (value-of exp3 env)))
      (proc-exp
       (var body)
       (proc-val var body env))
      (apply-exp
       (exp1 exp2)
       (let [(proc1 (value-of exp1 env))
             (val1 (value-of exp2 env))]
         (cases expval proc1
           (proc-val
            (var body saved-env)
            (value-of body (extend-env var (ref-val (newref val1)) saved-env)))
           (else (eopl:error "~s not a procedure" proc1)))))
      (let-exp
       (var exp1 body)
       (let [(val1 (value-of exp1 env))]
         (value-of body (extend-env var (ref-val (newref val1)) env))))
      (letrec-exp
       (var proc-var proc-body body)
       (let* ([loc (newref 0)]
             [new-env (extend-env var (ref-val loc) env)])
         (begin
           (setref! loc (proc-val proc-var proc-body new-env))
           (value-of body new-env))))
      (assign-exp
       (var exp)
       (begin
         (setref! (denval->ref (apply-env env var)) (value-of exp env))
         (num-val 27)))
      (array-exp
       (len exp)
       (array-val (newarray (expval->num (value-of len env)) (value-of exp env))))
      (arrayref-exp
       (exp1 exp2)
       (arrayref (expval->array (value-of exp1 env)) (expval->num (value-of exp2 env))))
      (arrayset-exp
       (exp1 exp2 exp3)
       (arrayset (expval->array (value-of exp1 env)) (expval->num (value-of exp2 env)) (value-of exp3 env)))
      (arraylength-exp
       (exp1) (arraylength (expval->array (value-of exp1 env))))
      (begin-exp
        (lst)
        (letrec ([f (lambda (lst val)
                      (if (null? lst)
                          val
                          (f (cdr lst) (value-of (car lst) env))))])
          (f lst 27))))))

(define run
  (lambda (e)
    (begin
      (initialize-store!)
      (value-of e (empty-env)))))

(define exp1
  (letrec-exp 'f
              'x
              (if-exp (zero?-exp (var-exp 'x))
                      (const-exp 1)
                      (mult-exp (var-exp 'x) (apply-exp (var-exp 'f) (diff-exp (var-exp 'x) (const-exp 1)))))
              (apply-exp (var-exp 'f) (const-exp 5))))

(define exp2
  (let-exp 'arr1
           (array-exp (const-exp 6) (const-exp 2))
           (begin-exp
             (list (arrayset-exp (var-exp 'arr1) (const-exp 3) (const-exp 7))
                   (arrayset-exp (var-exp 'arr1) (const-exp 4) (const-exp 8))
                   (arrayref-exp (var-exp 'arr1) (const-exp 4))))))
