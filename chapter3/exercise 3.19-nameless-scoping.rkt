#lang eopl

(define-datatype environment environment?
  (extend-env
   (var identifier?)
   (val expval?)
   (env environment?))
  (extend-env-letrec
   (var identifier?)
   (proc-var identifier?)
   (proc-body expression?)
   (env environment?))
  (empty-env))

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
      (else (report-no-binding-found 'apply-env search-var)))))

(define empty-senv
  (lambda ()
    '()))

(define extend-senv
  (lambda (var senv)
    (cons var senv)))

(define apply-senv
  (lambda (senv var)
    (cond
      ((null? senv)
       (report-no-binding-found 'apply-senv var))
      ((eqv? var (car senv)) 0)
      (else
       (+ 1 (apply-senv (cdr senv) var))))))

(define empty-nameless-env
  (lambda ()
    '()))

(define extend-nameless-env
  (lambda (val letrec? nenv)
    (cons (cons val letrec?) nenv)))

(define nameless-environment?
  (lambda (x)
    ((list-of expval?) x)))

(define apply-nameless-env
  (lambda (nameless-env n)
    (let [(val-pair (list-ref nameless-env n))]
      (if (cdr val-pair)
          (proc-val (procedure (car val-pair) nameless-env))
          (car val-pair)))))

(define report-no-binding-found
  (lambda (apply-name search-var)
    (eopl:error apply-name "No binding for ~s" search-var)))


(define-datatype program program?
  (a-program
   (exp1 expression?)))

(define identifier? symbol?)
    
(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (proc-val
   (proc proc?))
  (proc-rec-val
   (proc proc?)))

(define-datatype proc proc?
  (procedure
   (body expression?)
   (saved-env nameless-environment?)))

(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (body saved-nameless-env)
                 (value-of body
                           (extend-nameless-env val saved-nameless-env))))))

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

(define expval->proc-rec
  (lambda (val)
    (cases expval val
      (proc-rec-val (proc) proc)
      (else (report-expval-extractor-error 'procedure val)))))

(define init-env
  (lambda ()
    '()))

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
  (call-exp
   (proc expression?)
   (val expression?))
  (letrec-exp
   (var identifier?)
   (proc-var identifier?)
   (proc-body expression?)
   (body expression?))
  (nameless-var-exp
   (num number?))
  (nameless-let-exp
   (exp expression?)
   (body expression?))
  (nameless-proc-exp
   (body expression?))
  (nameless-letrec-exp
   (proc-body expression?)
   (body expression?)))

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (value-of exp1 (init-env))))))

(define translation-of
  (lambda (exp senv)
    (cases expression exp
      (const-exp (num) (const-exp num))
      (diff-exp (exp1 exp2)
                (diff-exp
                 (translation-of exp1 senv)
                 (translation-of exp2 senv)))
      (add-exp (exp1 exp2)
                (add-exp
                 (translation-of exp1 senv)
                 (translation-of exp2 senv)))
      (mult-exp (exp1 exp2)
                (mult-exp
                 (translation-of exp1 senv)
                 (translation-of exp2 senv)))
      (quot-exp (exp1 exp2)
                (quot-exp
                 (translation-of exp1 senv)
                 (translation-of exp2 senv)))
      (zero?-exp (exp1)
                 (zero?-exp
                  (translation-of exp1 senv)))
      (if-exp (exp1 exp2 exp3)
              (if-exp
               (translation-of exp1 senv)
               (translation-of exp2 senv)
               (translation-of exp3 senv)))
      (var-exp (var)
               (nameless-var-exp
                (apply-senv senv var)))
      (let-exp (var exp1 body)
               (nameless-let-exp
                (translation-of exp1 senv)
                (translation-of body
                                (extend-senv var senv))))
      (proc-exp (var body)
                (nameless-proc-exp
                 (translation-of body
                                 (extend-senv var senv))))
      (call-exp (rator rand)
                (call-exp
                 (translation-of rator senv)
                 (translation-of rand senv)))
      (letrec-exp (var proc-var proc-body body)
                  (nameless-letrec-exp
                   (translation-of proc-body (extend-senv var (extend-senv proc-var senv)))
                   (translation-of body
                                   (extend-senv var senv))))
      (else
       (report-invalid-source-expression exp)))))

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
      (zero?-exp (exp)
                 (bool-val (= 0 (expval->num (value-of exp env)))))
      (if-exp
       (exp1 exp2 exp3)
       (if (expval->bool (value-of exp1 env))
           (value-of exp2 env)
           (value-of exp3 env)))
      (call-exp
       (exp1 exp2)
       (let [(proc1 (expval->proc (value-of exp1 env)))
             (val1 (value-of exp2 env))]
         (apply-procedure proc1 val1)))
      (nameless-proc-exp
       (body)
       (proc-val (procedure body env)))
      (nameless-var-exp (num) (apply-nameless-env env num))
      (nameless-let-exp
       (exp1 body)
       (let [(val1 (value-of exp1 env))]
         (value-of body (extend-nameless-env val1 #f env))))
      (nameless-letrec-exp
       (proc-body body)
       (value-of body (extend-nameless-env (proc-rec-val proc-body) #t env)))
      (else
       (report-invalid-source-expression exp)))))

(define report-invalid-source-expression
  (lambda (exp)
    (eopl:error 'translation "Not a valid expression: ~s" exp)))

(define scan&parse '())

(define value
  (lambda (e)
    (value-of (translation-of e (empty-senv)) (empty-nameless-env))))

(define exp1
  (let-exp 'a
            (const-exp 3)
            (let-exp 'f
                     (proc-exp 'x
                               (add-exp (var-exp 'x) (var-exp 'a)))
                     (call-exp (var-exp 'f) (const-exp 2)))))

(define exp2
  (let-exp 'a
           (const-exp 3)
           (call-exp (proc-exp 'x (add-exp (var-exp 'a) (var-exp 'x)))
                     (const-exp 2))))

(define exp3
  (letrec-exp 'f
              'x
              (if-exp (zero?-exp (var-exp 'x))
                      (const-exp 1)
                      (mult-exp (var-exp 'x) (call-exp (var-exp 'f) (diff-exp (var-exp 'x) (const-exp 1)))))
              (call-exp (var-exp 'f) (const-exp 5))))
                                  
