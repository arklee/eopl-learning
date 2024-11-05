#lang eopl

(define empty-env
  (lambda () (list 'empty-env)))

(define extend-env
  (lambda (var val env)
    (list 'extend-env var val env)))

(define apply-env
  (lambda (env search-var)
    (cond
      ((eqv? (car env) 'empty-env) (report-no-binding-found search-var))
      ((eqv? (car env) 'extend-env)
       (let ((saved-var (cadr env))
             (saved-val (caddr env))
             (saved-env (cadddr env)))
         (if (eqv? search-var saved-var)
             saved-val
             (apply-env saved-env search-var))))
      (else
       (report-invalid-env env)))))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))


(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment: ~s" env)))

(define-datatype program program?
  (a-program
   (exp1 expression?)))

(define identifier? symbol?)

(define environment?
  (lambda (lst)
    (and (list? lst) (not (null? lst))
         (or (eqv? (car lst) 'empty-env)
             (eqv? (car lst) 'extend-env)))))

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
      (else (report-expval-extractor-error 'lst val)))))

(define expval->cdr
  (lambda (val)
    (cases expval val
      (pair-val (car cdr) cdr)
      (else (report-expval-extractor-error 'lst val)))))

(define init-env
  (lambda ()
    (extend-env
     'i (num-val 1)
     (extend-env
      'v (num-val 5)
      (extend-env
       'x (num-val 10)
       (empty-env))))))

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
   (val expression?)))

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (value-of exp1 (init-env))))))

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
         (num-val (- (expval->num val1) (expval->num val2)))))
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
                 (bool-val (= 0 (expval->bool (value-of exp env)))))
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
       (proc-val var body env))
      (apply-exp
       (exp1 exp2)
       (let [(proc1 (value-of exp1 env))
             (val1 (value-of exp2 env))]
         (cases expval proc1
           (proc-val
            (var body saved-env)
            (value-of body (extend-env var val1 saved-env)))
           (else (eopl:error "not a procedure"))))))))

(define scan&parse '())

(define exp1
  (value-of
   (let-exp 'f (proc-exp 'a (add-exp (var-exp 'a) (const-exp 1)))
            (apply-exp (var-exp 'f) (const-exp 2)))
   (empty-env)))

(display exp1)