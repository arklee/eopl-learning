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
   (proc-vars (list-of identifier?))
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
       (var proc-vars proc-body saved-env)
       (if (eqv? search-var var)
           (proc-marg-val (proc-vars) proc-body env)
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
  (pair-val
   (car expval?)
   (cdr expval?))
  (emptylist)
  (list-val
   (car expval?)
   (cdr expval?))
  (proc-marg-val
   (vars (list-of identifier?))
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
  ;; (mult-exp
  ;;  (exp1 expression?)
  ;;  (exp2 expression?))
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
  (list-exp
   (exps list?))
  (pair-exp
   (car expression?)
   (cdr expression?))
  (car-exp
   (exp1 expression?))
  (cdr-exp
   (exp1 expression?))
  (null?-exp
   (exp1 expression?))
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
  (letmul-exp
   (vars (list-of identifier?))
   (exps (list-of expression?))
   (body expression?))
  (proc-marg-exp
   (var (list-of identifier?))
   (body expression?))
  (call-exp
   (rator expression?)
   (rand (list-of expression?)))
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
      ;; (minus-exp (exp1)
      ;;            (num-val (- (expval->num (value-of exp1 env)))))
      ;; (add-exp
      ;;  (exp1 exp2)
      ;;  (let [(val1 (value-of exp1 env))
      ;;        (val2 (value-of exp2 env))]
      ;;    (num-val (+ (expval->num val1) (expval->num val2)))))
      ;; (mult-exp
      ;;  (exp1 exp2)
      ;;  (let [(val1 (value-of exp1 env))
      ;;        (val2 (value-of exp2 env))]
      ;;    (num-val (* (expval->num val1) (expval->num val2)))))
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
      (pair-exp (carexp cdrexp)
                (value-of/k carexp env (pair-car-cont cdrexp env cont)))
      (car-exp (exp1)
               (value-of/k exp1 env (car-cont cont)))
      (cdr-exp (exp1)
               (value-of/k exp1 env (cdr-cont cont)))
      (list-exp
       (exps)
       (value-of/k (car exps) env (list-cont (cdr exps) '() env cont)))
      (null?-exp (exp1)
               (value-of/k exp1 env (null?-cont cont)))
      (zero?-exp (exp)
                 (value-of/k exp env (zero1-cont cont)))
      (var-exp (var) (apply-cont cont (apply-env env var)))
      (let-exp
       (var exp1 body)
       (value-of/k exp1 env (let-exp-cont var body env cont)))
      (letmul-exp
       (vars exps body)
       (value-of/k (car exps) env (letmul-cont vars (cdr exps) body env cont)))
      (if-exp
       (exp1 exp2 exp3)
       (value-of/k exp1 env (if-test-cont exp2 exp3 env cont)))
      (proc-marg-exp
       (vars body)
       (apply-cont cont (proc-marg-val vars body env)))
      (letrec-exp
       (var proc-vars proc-body body)
       (value-of/k body (extend-env-letrec var proc-vars proc-body env) cont))
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

(define rator-cont
  (lambda (rand env cont)
    (lambda (proc1)
      (cases expval proc1
        (proc-marg-val (vars body saved-env)
                       (value-of/k (car rand) env (rand-marg-cont vars (cdr rand) body saved-env cont)))
        (else (eopl:error 'call-exp "~s is not a procedure" proc1))))))

(define rand-marg-cont
  (lambda (vars rest body env cont)
    (lambda (val)
      (let ([newenv (extend-env (car vars) val env)])
        (if (null? rest)
            (value-of/k body newenv cont)
            (value-of/k (car rest)
                        newenv
                        (letmul-cont (cdr vars) (cdr rest) body newenv cont)))))))

(define pair-car-cont
  (lambda (cdr-exp env cont)
    (lambda (car-val)
      (value-of/k cdr-exp env (pair-cdr-cont car-val cont)))))

(define pair-cdr-cont
  (lambda (car-val cont)
    (lambda (cdr-val)
      (apply-cont cont (list-val car-val cdr-val)))))

(define car-cont
  (lambda (cont)
    (lambda (val)
      (cases expval val
        (list-val (car cdr) (apply-cont cont car))
        (else (eopl:error "~s not a list" val))))))

(define cdr-cont
  (lambda (cont)
    (lambda (val)
      (cases expval val
        (list-val (car cdr) (apply-cont cont cdr))
        (else (eopl:error "~s not a list" val))))))

(define null?-cont
  (lambda (cont)
    (lambda (val)
      (cases expval val
        (list-val (car cdr) (apply-cont cont (bool-val #t)))
        (else (apply-cont cont (bool-val #f)))))))

(define list-cont
  (lambda (rest vals env cont)
    (lambda (firstval)
      (let ([allvals (cons firstval vals)])
        (if (null? rest)
            (list-cont-final allvals cont)
            (value-of/k (car rest) env (list-cont (cdr rest) allvals env cont)))))))

(define list-cont-final
  (lambda (vals cont)
    (letrec ([f (lambda (vals lst)
                  (if (null? vals)
                      lst
                      (f (cdr vals) (list-val (car vals) lst))))])
      (apply-cont cont (f vals (emptylist))))))
1
(define letmul-cont
  (lambda (vars rest body env cont)
    (lambda (val)
      (let ([newenv (extend-env (car vars) val env)])
        (if (null? rest)
            (value-of/k body newenv cont)
            (value-of/k (car rest)
                        newenv
                        (letmul-cont (cdr vars) (cdr rest) body newenv cont)))))))

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
           (proc-marg-exp '(x) (diff-exp (var-exp 'x) (const-exp 1)))
           (call-exp (var-exp 'f) (list (const-exp 3)))))

;; (define exp2
;;   (diff-exp (const-exp 3) (const-exp 2)))

(define exp3
  (list-exp (list (const-exp 3) (const-exp 4) (const-exp 5) (zero?-exp (const-exp 0)))))

(define exp4
  (let-exp 'f
           (proc-marg-exp '(x y z)
                          (diff-exp (diff-exp (var-exp 'x) (var-exp 'y)) (var-exp 'z)))
           (call-exp (var-exp 'f) (list [const-exp 10]
                                        [const-exp 3]
                                        [const-exp 2]))))

(display (run exp1))
