#lang eopl

(define identifier? symbol?)

(define exp 'uninitialized)
(define val 'uninitialized)
(define cont 'uninitialized)
(define env 'uninitialized)
(define proc 'uninitialized)

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
           (proc-val proc-var proc-body env)
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

(define expval->bool
  (lambda (val)
    (cases expval val
      (bool-val (bool) bool)
      (else (report-expval-extractor-error 'bool val)))))

(define-datatype expression expression?
  (const-exp
   (num number?))
  (diff-exp
   (exp1 expression?)
   (exp2 expression?))
  (mult-exp
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
  (call-exp
   (rator expression?)
   (rand expression?))
  (letrec-exp
   (var identifier?)
   (proc-var identifier?)
   (proc-body expression?)
   (body expression?)))

(define value-of/k
  (lambda ()
    (cases expression exp
      (const-exp (num)
                 (set! val (num-val num))
                 (apply-cont))
      (diff-exp
       (exp1 exp2)
       (set! exp exp1)
       (set! cont (diff1-cont exp2 cont))
       (value-of/k))
      (mult-exp
       (exp1 exp2)
       (set! exp exp1)
       (set! cont (mult1-cont exp2 cont))
       (value-of/k))
      (zero?-exp
       (exp1)
       (set! exp exp1)
       (set! cont (zero1-cont cont))
       (value-of/k))
      (var-exp
       (var)
       (set! val (apply-env env var))
       (apply-cont))
      (let-exp
       (var exp1 body)
       (set! exp exp1)
       (set! cont (let-exp-cont var body cont))
       (value-of/k))
      (if-exp
       (exp1 exp2 exp3)
       (set! exp exp1)
       (set! cont (if-test-cont exp2 exp3 cont))
       (value-of/k))
      (proc-exp
       (var body)
       (set! val (proc-val var body env))
       (apply-cont))
      (letrec-exp
       (var proc-var proc-body body)
       (set! exp body)
       (set! env (extend-env-letrec var proc-var proc-body env))
       (value-of/k))
      (call-exp
       (rator rand)
       (set! exp rator)
       (set! cont (rator-cont rand cont))
       (value-of/k)))))

(define-datatype continuation continuation?
  (end-cont)
  (zero1-cont
   (cont continuation?))
  (let-exp-cont
   (var identifier?)
   (body expression?)
   (cont continuation?))
  (if-test-cont
   (exp2 expression?)
   (exp3 expression?)
   (cont continuation?))
  (diff1-cont
   (exp2 expression?)
   (cont continuation?))
  (diff2-cont
   (val expval?)
   (cont continuation?))
  (mult1-cont
   (exp2 expression?)
   (cont continuation?))
  (mult2-cont
   (val expval?)
   (cont continuation?))
  (rator-cont
   (rand expression?)
   (cont continuation?))
  (rand-cont
   (proc1 expval?)
   (cont continuation?)))

(define apply-cont
  (lambda ()
    (cases continuation cont
      (end-cont ()
                (begin (eopl:printf "End of continuation. ~%")
                       val))
      (zero1-cont (saved-cont)
                  (set! cont saved-cont)
                  (set! val (bool-val (zero? (expval->num val))))
                  (apply-cont))
      (let-exp-cont (var body saved-cont)
                    (set! exp body)
                    (set! env (extend-env var val env))
                    (set! cont saved-cont)
                    (value-of/k))
      (if-test-cont (exp2 exp3 saved-cont)
                    (if (expval->bool val)
                        (set! exp exp2)
                        (set! exp exp3))
                    (set! cont saved-cont)
                    (value-of/k))
      (diff1-cont (exp2 saved-cont)
                  (set! exp exp2)
                  (set! cont (diff2-cont val saved-cont))
                  (value-of/k))
      (diff2-cont (val1 saved-cont)
                  (let ([num1 (expval->num val1)]
                        [num2 (expval->num val)])
                    (set! val (num-val (- num1 num2)))
                    (set! cont saved-cont)
                    (apply-cont)))
      (mult1-cont (exp2 saved-cont)
                  (set! exp exp2)
                  (set! cont (mult2-cont val cont))
                  (value-of/k))
      (mult2-cont (val1 saved-cont)
                  (let ([num1 (expval->num val1)]
                        [num2 (expval->num val)])
                    (set! val (num-val (* num1 num2)))
                    (set! cont saved-cont)
                    (apply-cont)))
      (rator-cont (rand saved-cont)
                  (set! exp rand)
                  (set! cont (rand-cont val saved-cont))
                  (value-of/k))
      (rand-cont (proc1 saved-cont)
                 (set! proc proc1)
                 (set! cont saved-cont)
                 (apply-procedure/k)))))

(define apply-procedure/k
  (lambda ()
    (cases expval proc
      (proc-val
       (var body saved-env)
       (set! exp body)
       (set! env (extend-env var val saved-env))
       (value-of/k))
      (else (eopl:error 'call-exp "~s is not a procedure" exp)))))
      
(define run
  (lambda (e)
    (set! env (empty-env))
    (set! cont (end-cont))
    (set! exp e)
    (value-of/k)))

(define exp-fact
  (letrec-exp 'f
              'x
              (if-exp (zero?-exp (var-exp 'x))
                      (const-exp 1)
                      (mult-exp (var-exp 'x) (call-exp (var-exp 'f) (diff-exp (var-exp 'x) (const-exp 1)))))
              (call-exp (var-exp 'f) (const-exp 5))))

(define exp-fact-iter
  (letrec-exp 'f
              'x
              (proc-exp 'n
                        (if-exp (zero?-exp (var-exp 'x))
                                (var-exp 'n)
                                (call-exp (call-exp (var-exp 'f)
                                                    (diff-exp (var-exp 'x) (const-exp 1)))
                                          (mult-exp (var-exp 'n) (var-exp 'x)))))
              (call-exp (call-exp (var-exp 'f) (const-exp 5))
                        (const-exp 1))))

(define exp1
  (let-exp 'f
            (proc-exp 'x (diff-exp (var-exp 'x) (const-exp 1)))
            (call-exp (var-exp 'f) (const-exp 3))))

(define exp2
  (diff-exp (const-exp 3) (const-exp 2)))

(display (run exp-fact))
