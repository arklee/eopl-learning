#lang eopl

(define identifier? symbol?)

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
  ;; (minus-exp
  ;;  (exp1 expression?))
  ;; (add-exp
  ;;  (exp1 expression?)
  ;;  (exp2 expression?))
  (mult-exp
   (exp1 expression?)
   (exp2 expression?))
  ;; (quot-exp
  ;;  (exp1 expression?)
  ;;  (exp2 expression?))
  ;; (equal?-exp
  ;;  (exp1 expression?)
  ;;  (exp2 expression?))
  ;; (greater?-exp
  ;;  (exp1 expression?)
  ;;  (exp2 expression?))
  ;; (less?-exp
  ;;  (exp1 expression?)
  ;;  (exp2 expression?))
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
  (lambda (exp env cont)
    (cases expression exp
      (const-exp (num) (apply-cont cont (num-val num)))
      (diff-exp
       (exp1 exp2)
       (value-of/k exp1 env (diff1-cont exp2 env cont)))
      (mult-exp
       (exp1 exp2)
       (value-of/k exp1 env (mult1-cont exp2 env cont)))
      ;; (minus-exp (exp1)
      ;;            (num-val (- (expval->num (value-of exp1 env)))))
      ;; (add-exp
      ;;  (exp1 exp2)
      ;;  (let [(val1 (value-of exp1 env))
      ;;        (val2 (value-of exp2 env))]
      ;;    (num-val (+ (expval->num val1) (expval->num val2)))))
      ;; (quot-exp
      ;;  (exp1 exp2)
      ;;  (let [(val1 (value-of exp1 env))
      ;;        (val2 (value-of exp2 env))]
      ;;    (num-val (quotient (expval->num val1) (expval->num val2)))))
      ;; (equal?-exp
      ;;  (exp1 exp2)
      ;;  (let [(val1 (value-of exp1 env))
      ;;        (val2 (value-of exp2 env))]
      ;;    (bool-val (= (expval->num val1) (expval->num val2)))))
      ;; (greater?-exp
      ;;  (exp1 exp2)
      ;;  (let [(val1 (value-of exp1 env))
      ;;        (val2 (value-of exp2 env))]
      ;;    (bool-val (> (expval->num val1) (expval->num val2)))))
      ;; (less?-exp
      ;;  (exp1 exp2)
      ;;  (let [(val1 (value-of exp1 env))
      ;;        (val2 (value-of exp2 env))]
      ;;    (bool-val (< (expval->num val1) (expval->num val2)))))
      (zero?-exp (exp)
                 (value-of/k exp env (zero1-cont cont)))
      (var-exp (var) (apply-cont cont (apply-env env var)))
      (let-exp
       (var exp1 body)
       (value-of/k exp1 env (let-exp-cont var body env cont)))
      (if-exp
       (exp1 exp2 exp3)
       (value-of/k exp1 env (if-test-cont exp2 exp3 env cont)))
      (proc-exp
       (var body)
       (apply-cont cont (proc-val var body env)))
      (letrec-exp
       (var proc-var proc-body body)
       (value-of/k body (extend-env-letrec var proc-var proc-body env) cont))
      (call-exp (rator rand)
                (value-of/k rator env (rator-cont rand env cont))))))

(define apply-cont
  (lambda (cont val)
    (cont val)))

(define end-cont
  (lambda ()
    (lambda (val)
      (begin (eopl:printf "End of continuation. ~%")
             val))))

(define zero1-cont
  (lambda (cont)
    (lambda (val)
      (apply-cont cont (bool-val (zero? (expval->num val)))))))

(define let-exp-cont
  (lambda (var body env cont)
    (lambda (val)
      (value-of/k body (extend-env var val env) cont))))

(define if-test-cont
  (lambda (exp2 exp3 env cont)
    (lambda (val)
      (if (expval->bool val)
          (value-of/k exp2 env cont)
          (value-of/k exp3 env cont)))))

(define diff1-cont
  (lambda (exp2 env cont)
    (lambda (val1)
      (value-of/k exp2 env (diff2-cont val1 cont)))))

(define diff2-cont
  (lambda (val1 cont)
    (lambda (val2)
      (let ([num1 (expval->num val1)]
            [num2 (expval->num val2)])
        (apply-cont cont (num-val (- num1 num2)))))))

(define mult1-cont
  (lambda (exp2 env cont)
    (lambda (val1)
      (value-of/k exp2 env (mult2-cont val1 cont)))))

(define mult2-cont
  (lambda (val1 cont)
    (lambda (val2)
      (let ([num1 (expval->num val1)]
            [num2 (expval->num val2)])
        (apply-cont cont (num-val (* num1 num2)))))))

(define rator-cont
  (lambda (rand env cont)
    (lambda (proc1)
      (value-of/k rand env (rand-cont proc1 cont)))))

(define rand-cont
  (lambda (proc1 cont)
    (lambda (val)
      (cases expval proc1
        (proc-val
         (var body saved-env)
         (value-of/k body (extend-env var val saved-env) cont))
        (else (eopl:error 'call-exp "~s is not a procedure" proc1))))))

(define run
  (lambda (e)
    (value-of/k e (empty-env) (end-cont))))

;; (define exp-fact
;;   (letrec-exp 'f
;;               'x
;;               (if-exp (zero?-exp (var-exp 'x))
;;                       (const-exp 1)
;;                       (mult-exp (var-exp 'x) (apply-exp (var-exp 'f) (diff-exp (var-exp 'x) (const-exp 1)))))
;;               (apply-exp (var-exp 'f) (const-exp 5))))
(define exp1
  (let-exp 'f
            (proc-exp 'x
                      (let-exp 'y
                               (diff-exp (var-exp 'x) (const-exp 1))
                               (mult-exp (var-exp 'y) (const-exp 2))))
            (call-exp (var-exp 'f) (const-exp 3))))

;; (define exp2
;;   (diff-exp (const-exp 3) (const-exp 2)))

(display (run exp1))
