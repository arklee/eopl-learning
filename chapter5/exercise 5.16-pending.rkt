#lang eopl
(require racket/match)

(define identifier? symbol?)

(define-datatype environment environment?
  (empty-env)
  (extend-env
   (var identifier?)
   (val integer?)
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

(define-datatype program program?
  (a-program
   (exp1 expression?)))

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
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

(define empty-store
  (lambda () '()))

(define expval->bool
  (lambda (val)
    (cases expval val
      (bool-val (bool) bool)
      (else (report-expval-extractor-error 'bool val)))))

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

(define-datatype expression expression?
  (const-exp
   (num number?))
  (diff-exp
   (exp1 expression?)
   (exp2 expression?))
  (add-exp
   (exp1 expression?)
   (exp2 expression?))
  (mult-exp
   (exp1 expression?)
   (exp2 expression?))
  (zero?-exp
   (exp1 expression?))
  (not-exp
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
  (call-exp
   (rator expression?)
   (rand expression?))
  (letrec-exp
   (var identifier?)
   (proc-var identifier?)
   (proc-body expression?)
   (body expression?)))

(define value-of/k
  (lambda (exp env cont)
    (cases expression exp
      (const-exp (num) (apply-cont cont (num-val num)))
      (diff-exp
       (exp1 exp2)
       (value-of/k exp1 env (cons (list 'diff1-frame exp2 env) cont)))
      (add-exp
       (exp1 exp2)
       (value-of/k exp1 env (cons (list 'add1-frame exp2 env) cont)))
      (mult-exp
       (exp1 exp2)
       (value-of/k exp1 env (cons (list 'mult1-frame exp2 env) cont)))
      (zero?-exp (exp)
                 (value-of/k exp env (cons (list 'zero1-frame) cont)))
      (not-exp (exp)
                 (value-of/k exp env (cons (list 'not-frame) cont)))
      (var-exp (var) (apply-cont cont (deref (apply-env env var))))
      (let-exp
       (var exp1 body)
       (value-of/k exp1 env (cons (list 'let-exp-frame var body env) cont)))
      (if-exp
       (exp1 exp2 exp3)
       (value-of/k exp1 env (cons (list 'if-test-frame exp2 exp3 env) cont)))
      (proc-exp
       (var body)
       (apply-cont cont (proc-val var body env)))
      (letrec-exp (var proc-var proc-body body)
                  (let* ([loc (newref (num-val 27))]
                         [new-env (extend-env var loc env)])
                    (begin
                      (setref! loc (proc-val proc-var proc-body new-env))
                      (value-of/k body new-env cont))))
      (call-exp (rator rand)
                (value-of/k rator env (cons (list 'rator-frame rand env) cont))))))

(define end-cont
  (lambda ()
    '()))

(define apply-cont
  (lambda (cont val)
    (if (null? cont)
        (begin (eopl:printf "End of continuation. ~%")
               val)
        (let ([frame (car cont)]
              [saved-cont (cdr cont)])
          (match frame
            [(list type)
             #:when (eqv? type 'zero1-frame)
             (apply-cont saved-cont (bool-val (zero? (expval->num val))))]
            [(list type)
             #:when (eqv? type 'not-frame)
             (apply-cont saved-cont (bool-val (not (expval->bool val))))]
            [(list type var body env)
             #:when (eqv? type 'let-exp-frame)
             (value-of/k body (extend-env var (newref val) env) saved-cont)]
            [(list type exp2 exp3 env)
             #:when (eqv? type 'if-test-frame)
             (if (expval->bool val)
                 (value-of/k exp2 env saved-cont)
                 (value-of/k exp3 env saved-cont))]
            [(list type exp2 env)
             #:when (eqv? type 'diff1-frame)
             (value-of/k exp2 env (cons (list 'diff2-frame val) saved-cont))]
            [(list type val1)
             #:when (eqv? type 'diff2-frame)
             (let ([num1 (expval->num val1)]
                   [num2 (expval->num val)])
               (apply-cont saved-cont (num-val (- num1 num2))))]
            [(list type exp2 env)
             #:when (eqv? type 'add1-frame)
             (value-of/k exp2 env (cons (list 'add2-frame val) saved-cont))]
            [(list type val1)
             #:when (eqv? type 'add2-frame)
             (let ([num1 (expval->num val1)]
                   [num2 (expval->num val)])
               (apply-cont saved-cont (num-val (+ num1 num2))))]
            [(list type exp2 env)
             #:when (eqv? type 'mult1-frame)
             (value-of/k exp2 env (cons (list 'mult2-frame val) saved-cont))]
            [(list type val1)
             #:when (eqv? type 'mult2-frame)
             (let ([num1 (expval->num val1)]
                   [num2 (expval->num val)])
               (apply-cont saved-cont (num-val (* num1 num2))))]
            [(list type rand env)
             #:when (eqv? type 'rator-frame)
             (value-of/k rand env (cons (list 'rand-frame val) saved-cont))]
            [(list type proc1)
             #:when (eqv? type 'rand-frame)
             (cases expval proc1
               (proc-val
                (var body saved-env)
                (value-of/k body (extend-env var (newref val) saved-env) saved-cont))
               (else (eopl:error 'call-exp "~s is not a procedure" proc1)))])))))
      
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
  (do-while-stm
   (exp expression?)
   (stm statement?))
  (var-stm
   (vars (list-of identifier?))
   (stm statement?))
  (pass-stm))

(define result-of/k
  (lambda (statem env cont)
    (cases statement statem
      (var-stm
       (vars stm1)
       (letrec ([create-var
                 (lambda (vars saved-env)
                   (if (null? vars)
                       saved-env
                       (extend-env (car vars) (newref 0) (create-var (cdr vars) saved-env))))])
         (result-of/k stm1 (create-var vars env) cont)))
      (set-stm
       (var exp)
       (setref! (apply-env env var) (value-of/k exp env cont)))
      (print-stm (exp)
       (display (value-of/k exp env cont)))
      (begin-stm (stms1)
                 (letrec ([run-stms
                           (lambda (stms)
                             (if (null? stms)
                                 (num-val 27)
                                 (begin (result-of/k (car stms) env cont)
                                        (run-stms (cdr stms)))))])
                   (run-stms stms1)))
      (if-stm
       (exp stm1 stm2)
       (if (expval->bool (value-of/k exp env cont))
           (result-of/k stm1 env cont)
           (result-of/k stm2 env cont)))
      (while-stm
       (exp stm1)
       (letrec ([while
                 (lambda (exp stm)
                   (if (expval->bool (value-of/k exp env cont))
                       (begin
                         (result-of/k stm env cont)
                         (while exp stm))
                       (pass-stm)))])
         (while exp stm1)))
      (do-while-stm
       (exp stm1)
       (letrec ([while
                 (lambda (exp stm)
                   (begin
                     (result-of/k stm env cont)
                     (if (expval->bool (value-of/k exp env cont))
                         (while exp stm)
                         (pass-stm))))])
         (while exp stm1)))
      (pass-stm () 27))))

(define run
  (lambda (stm)
    (begin
      (initialize-store!)
      (result-of/k stm (empty-env) (end-cont)))))

(define stm1
  (var-stm '(x y z)
           (begin-stm
             (list (set-stm 'x (const-exp 3))
                   (set-stm 'y (const-exp 4))
                   (set-stm 'z (const-exp 0))
                   (while-stm (not-exp (zero?-exp (var-exp 'x)))
                              (begin-stm
                                (list (set-stm 'z (add-exp (var-exp 'z) (var-exp 'y)))
                                      (set-stm 'x (diff-exp (var-exp 'x) (const-exp 1))))))
                   (print-stm (var-exp 'z))))))

(define stm2
  (var-stm '(x y z)
           (begin-stm
             (list (set-stm 'x (const-exp 3))
                   (set-stm 'y (const-exp 4))
                   (set-stm 'z (const-exp 0))
                   (do-while-stm (not-exp (zero?-exp (var-exp 'x)))
                              (begin-stm
                                (list (set-stm 'z (add-exp (var-exp 'z) (var-exp 'y)))
                                      (set-stm 'x (diff-exp (var-exp 'x) (const-exp 1))))))
                   (print-stm (var-exp 'z))))))

(run stm1)