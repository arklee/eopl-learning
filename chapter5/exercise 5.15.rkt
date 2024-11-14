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
  (mult-exp
   (exp1 expression?)
   (exp2 expression?))
  ;; (minus-exp
  ;;  (exp1 expression?))
  ;; (add-exp
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
       (value-of/k exp1 env (cons (diff1-frame exp2 env) cont)))
      (mult-exp
       (exp1 exp2)
       (value-of/k exp1 env (cons (mult1-frame exp2 env) cont)))
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
                 (value-of/k exp env (cons (zero1-frame) cont)))
      (var-exp (var) (apply-cont cont (apply-env env var)))
      (let-exp
       (var exp1 body)
       (value-of/k exp1 env (cons (let-exp-frame var body env) cont)))
      (if-exp
       (exp1 exp2 exp3)
       (value-of/k exp1 env (cons (if-test-frame exp2 exp3 env) cont)))
      (proc-exp
       (var body)
       (apply-cont cont (proc-val var body env)))
      (letrec-exp
       (var proc-var proc-body body)
       (value-of/k body (extend-env-letrec var proc-var proc-body env) cont))
      (call-exp (rator rand)
                (value-of/k rator env (cons (rator-frame rand env) cont))))))

(define-datatype frame frame?
  (zero1-frame)
  (let-exp-frame
   (var identifier?)
   (body expression?)
   (env environment?))
  (if-test-frame
   (exp2 expression?)
   (exp3 expression?)
   (env environment?))
  (diff1-frame
   (exp2 expression?)
   (env environment?))
  (diff2-frame
   (val expval?))
  (mult1-frame
   (exp2 expression?)
   (env environment?))
  (mult2-frame
   (val expval?))
  (rator-frame
   (rand expression?)
   (env environment?))
  (rand-frame
   (proc1 expval?)))

(define end-cont
  (lambda ()
    '()))

(define apply-cont
  (lambda (cont val)
    (if (null? cont)
        (begin (eopl:printf "End of continuation. ~%")
               val)
        (let ([frame1 (car cont)]
              [saved-cont (cdr cont)])
          (cases frame frame1
            (zero1-frame ()
                        (apply-cont saved-cont (bool-val (zero? (expval->num val)))))
            (let-exp-frame (var body env)
                          (value-of/k body (extend-env var val env) saved-cont))
            (if-test-frame (exp2 exp3 env)
                          (if (expval->bool val)
                              (value-of/k exp2 env saved-cont)
                              (value-of/k exp3 env saved-cont)))
            (diff1-frame (exp2 env)
                        (value-of/k exp2 env (cons (diff2-frame val) saved-cont)))
            (diff2-frame (val1)
                        (let ([num1 (expval->num val1)]
                              [num2 (expval->num val)])
                          (apply-cont saved-cont (num-val (- num1 num2)))))
            (mult1-frame (exp2 env)
                        (value-of/k exp2 env (cons (mult2-frame val) saved-cont)))
            (mult2-frame (val1)
                        (let ([num1 (expval->num val1)]
                              [num2 (expval->num val)])
                          (apply-cont saved-cont (num-val (* num1 num2)))))
            (rator-frame (rand env)
                        (value-of/k rand env (cons (rand-frame val) saved-cont)))
            (rand-frame (proc1)
                       (cases expval proc1
                         (proc-val
                          (var body saved-env)
                          (value-of/k body (extend-env var val saved-env) saved-cont))
                         (else (eopl:error 'call-exp "~s is not a procedure" proc1)))))))))
      
(define run
  (lambda (e)
    (value-of/k e (empty-env) (end-cont))))

(define exp-fact
  (letrec-exp 'f
              'x
              (if-exp (zero?-exp (var-exp 'x))
                      (const-exp 1)
                      (mult-exp (var-exp 'x) (call-exp (var-exp 'f) (diff-exp (var-exp 'x) (const-exp 1)))))
              (call-exp (var-exp 'f) (const-exp 8))))

(define exp-fact-iter
  (letrec-exp 'f
              'x
              (proc-exp 'n
                        (if-exp (zero?-exp (var-exp 'x))
                                (var-exp 'n)
                                (call-exp (call-exp (var-exp 'f)
                                                    (diff-exp (var-exp 'x) (const-exp 1)))
                                          (mult-exp (var-exp 'n) (var-exp 'x)))))
              (call-exp (call-exp (var-exp 'f) (const-exp 8))
                        (const-exp 1))))

(define exp1
  (let-exp 'f
            (proc-exp 'x (diff-exp (var-exp 'x) (const-exp 1)))
            (call-exp (var-exp 'f) (const-exp 3))))

(define exp2
  (diff-exp (const-exp 3) (const-exp 2)))

(display (run exp2))
