#lang eopl

(define-datatype environment environment?
  (empty-env)
  (extend-env
   (var identifier?)
   (loc number?)
   (env environment?)))

(define apply-env
  (lambda (env search-var)
    (cases environment env
      (empty-env () (report-no-binding-found search-var))
      (extend-env
       (saved-var loc saved-env)
       (if (eqv? search-var saved-var)
           loc
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
      (set! the-store (append the-store (list val)))
      next-ref)))

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
    (eopl:error 'setref! "No reference for ~s in ~s" ref the-store)))

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

(define value-of-operand
  (lambda (exp env)
    (cases expression exp
      (var-exp (v) (apply-env env v))
      (else (newref (value-of exp env))))))

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
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
  (proc-exp
   (var identifier?)
   (body expression?))
  (apply-exp
   (proc expression?)
   (val expression?))
  (letrec-exp
   (var identifier?)
   (proc-var identifier?)
   (proc-body expression?)
   (body expression?))
  (assign-exp
   (var identifier?)
   (val expression?))
  (begin-exp
    (exp1 (list-of expression?))))

(define value-of
  (lambda (exp env)
    (cases expression exp
      (const-exp (num) (num-val num))
      (diff-exp (exp1 exp2)
                (let [(val1 (value-of exp1 env))
                      (val2 (value-of exp2 env))]
                  (num-val (- (expval->num val1) (expval->num val2)))))
      (minus-exp (exp1)
                 (num-val (- (expval->num (value-of exp1 env)))))
      (add-exp (exp1 exp2)
               (let [(val1 (value-of exp1 env))
                     (val2 (value-of exp2 env))]
                 (num-val (+ (expval->num val1) (expval->num val2)))))
      (mult-exp (exp1 exp2)
                (let [(val1 (value-of exp1 env))
                      (val2 (value-of exp2 env))]
                  (num-val (* (expval->num val1) (expval->num val2)))))
      (quot-exp (exp1 exp2)
                (let [(val1 (value-of exp1 env))
                      (val2 (value-of exp2 env))]
                  (num-val (quotient (expval->num val1) (expval->num val2)))))
      (equal?-exp (exp1 exp2)
                  (let [(val1 (value-of exp1 env))
                        (val2 (value-of exp2 env))]
                    (bool-val (= (expval->num val1) (expval->num val2)))))
      (greater?-exp (exp1 exp2)
                    (let [(val1 (value-of exp1 env))
                          (val2 (value-of exp2 env))]
                      (bool-val (> (expval->num val1) (expval->num val2)))))
      (less?-exp (exp1 exp2)
                 (let [(val1 (value-of exp1 env))
                       (val2 (value-of exp2 env))]
                   (bool-val (< (expval->num val1) (expval->num val2)))))
      (zero?-exp (exp)
                 (bool-val (= 0 (expval->num (value-of exp env)))))
      (var-exp (var) (deref (apply-env env var)))
      (let-exp (var exp1 body)
               (let [(val1 (value-of exp1 env))]
                 (value-of body (extend-env var (newref val1) env))))
      (if-exp (exp1 exp2 exp3)
              (if (expval->bool (value-of exp1 env))
                  (value-of exp2 env)
                  (value-of exp3 env)))
      (proc-exp (var body)
                (proc-val (procedure var body env)))
      (letrec-exp (var proc-var proc-body body)
                  (let* ([loc (newref (num-val 27))]
                         [new-env (extend-env var loc env)])
                    (begin
                      (setref! loc (proc-val (procedure proc-var proc-body new-env)))
                      (value-of body new-env))))
      (apply-exp (exp1 exp2)
                 (let [(proc1 (expval->proc (value-of exp1 env)))
                       (val1 (value-of-operand exp2 env))]
                   (apply-procedure proc1 val1)))
      (assign-exp (var exp)
                    (begin
                      (setref! (apply-env env var)
                             (value-of exp env))
                      (num-val 27)))
      (begin-exp
        (lst)
        (letrec ([f (lambda (lst val)
                      (if (null? lst)
                          val
                          (f (cdr lst) (value-of (car lst) env))))])
          (f lst 27))))))

(define value
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
  (let-exp 'a
           (const-exp 1)
           (let-exp 'f
                    (proc-exp 'x
                              (assign-exp 'x (const-exp 5)))
                    (begin-exp (list (apply-exp (var-exp 'f) (var-exp 'a))
                                     (var-exp 'a))))))

(define exp3
  (let-exp 'n
           (const-exp 5)
           (let-exp 'a
                    (const-exp 5)
                    (letrec-exp 'f
                                'x
                                (begin-exp
                                  (list (assign-exp 'x (mult-exp (var-exp 'x) (const-exp 2)))
                                        (assign-exp 'a (diff-exp (var-exp 'a) (const-exp 1)))
                                        (if-exp (greater?-exp (var-exp 'a) (const-exp 0))
                                                (apply-exp (var-exp 'f) (var-exp 'x))
                                                (const-exp 27))))
                                (begin-exp
                                  (list
                                   (apply-exp (var-exp 'f) (var-exp 'n))
                                   (var-exp 'n)))))))

(display (value exp3))
