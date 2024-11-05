#lang eopl

(define identifier? symbol?)

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
  (pair-val
   (car expval?)
   (cdr expval?))
  (emptylist)
  (proc-val
   (var identifier?)
   (body expression?)
   (saved-env environment?)))

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
  (not-exp
   (exp1 expression?))
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
  (letrec-exp
   (var identifier?)
   (proc-var identifier?)
   (proc-body expression?)
   (body expression?))
  (begin-exp
    (exp1 expression?)
    (exp2 expression?))
  (assign-exp
   (var identifier?)
   (exp expression?)))

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
      (var-exp (var) (deref (apply-env env var)))
      (let-exp
       (var exp1 body)
       (let [(val1 (value-of exp1 env))]
         (value-of body (extend-env var (newref val1) env))))
      (if-exp
       (exp1 exp2 exp3)
       (if (expval->bool (value-of exp1 env))
           (value-of exp2 env)
           (value-of exp3 env)))
      (not-exp (exp1)
               (bool-val (not (expval->bool (value-of exp1 env)))))
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
       (proc-val var body env))
      (letrec-exp
       (var proc-var proc-body body)
       (let* ([loc (newref 0)]
             [new-env (extend-env var loc env)])
         (begin
           (setref! loc (proc-val proc-var proc-body new-env))
           (value-of body new-env))))
      (apply-exp
       (exp1 exp2)
       (let [(proc1 (value-of exp1 env))
             (val1 (value-of exp2 env))]
         (cases expval proc1
           (proc-val
            (var body saved-env)
            (value-of body (extend-env var (newref val1) saved-env)))
           (else (eopl:error "~s not a procedure" proc1)))))
      (assign-exp
       (var exp)
       (begin
         (setref! (apply-env env var) (value-of exp env))
         (num-val 27)))
      (begin-exp
        (exp1 exp2)
        (begin (value-of exp1 env)
               (value-of exp2 env))))))

(define-datatype statement statement?
  (set-stm
   (var identifier?)
   (exp expression?))
  (print-stm
   (exp expression?))
  (begin-stm
    (stms (list-of statement?)))
  (if-stm
   (exp expression?)
   (stm1 statement?)
   (stm2 statement?))
  (while-stm
   (exp expression?)
   (stm statement?))
  (var-stm
   (vars (list-of identifier?)))
  (pass-stm))

(define result-of
  (lambda (statem env)
    (cases statement statem
      (var-stm
       (vars)
       (letrec ([create-var
                 (lambda (vars saved-env)
                   (if (null? vars)
                       saved-env
                       (extend-env (car vars) (newref 0) (create-var (cdr vars) saved-env))))])
         (create-var vars env)))
      (set-stm
       (var exp)
       (begin
          (setref! (apply-env env var) (value-of exp env))
          env))
      (print-stm (exp)
                 (begin
                    (display (value-of exp env))
                    (display "\n")
                    env))
      (begin-stm (stms1)
                 (letrec ([run-stms
                           (lambda (stms env)
                             (if (null? stms)
                                 env
                                 (run-stms (cdr stms) (result-of (car stms) env))))])
                   (run-stms stms1 env)))
      (if-stm
       (exp stm1 stm2)
       (if (expval->bool (value-of exp env))
           (result-of stm1 env)
           (result-of stm2 env)))
      (while-stm
       (exp stm1)
       (letrec ([while
                 (lambda (exp stm)
                   (if (expval->bool (value-of exp env))
                       (begin
                         (result-of stm env)
                         (while exp stm))
                       env))])
         (while exp stm1)))
      (pass-stm () env))))
             
(define run
  (lambda (p)
    (begin
      (initialize-store!)
      (cond [(expression? p) (value-of p (empty-env))]
            [(statement? p) (result-of p (empty-env))]
            [else (eopl:error "~s is not a valid program" p)]))))

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
                              (begin-exp (assign-exp 'a (const-exp 2))
                                         (add-exp (var-exp 'a) (var-exp 'x))))
                    (begin-exp
                      (assign-exp 'a (const-exp 3))
                      (apply-exp (var-exp 'f) (const-exp 5))))))

(define stm1
  (begin-stm
    (list [var-stm '(x y temp)]
          [set-stm 'x (const-exp 1)]
          [set-stm 'y (const-exp 1)]
          [while-stm (less?-exp (var-exp 'x) (const-exp 50))
                     (begin-stm
                       (list [set-stm 'temp (var-exp 'x)]
                             [print-stm (var-exp 'x)]
                             [set-stm 'x (var-exp 'y)]
                             [set-stm 'y (add-exp (var-exp 'x) (var-exp 'temp))]))])))

(define stm2
  (begin-stm
    (list (var-stm '(x))
          (set-stm 'x (const-exp 3))
          (var-stm '(y z))
          (set-stm 'y (const-exp 2))
          (set-stm 'z (const-exp 4))
          (print-stm (add-exp (var-exp 'x) (add-exp (var-exp 'y) (var-exp 'z)))))))