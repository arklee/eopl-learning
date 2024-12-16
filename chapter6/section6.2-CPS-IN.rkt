#lang eopl

(define identifier? symbol?)

(define empty-env
  (lambda ()
    '()))

(define empty-env?
  (lambda (x) (null? x)))

(define extend-env*
  (lambda (syms vals old-env)
    (cons (list 'let syms vals) old-env)))

(define extend-env-rec**
  (lambda (p-names b-varss p-bodies saved-env)
    (cons
     (list 'letrec p-names b-varss p-bodies)
     saved-env)))

(define apply-env
  (lambda (env search-sym)
    (if (null? env)
        (eopl:error 'apply-env "No binding for ~s" search-sym)
        (let* ((binding (car env))
               (saved-env (cdr env)))
          (let ((pos (list-index search-sym (cadr binding))))
            (if pos
                (case (car binding)
                  ((let)
                   (list-ref (caddr binding) pos))
                  ((letrec)
                   (let ((bvars (caddr binding))
                         (bodies (cadddr binding)))
                     (proc-val
                      (procedure
                       (list-ref bvars pos)
                       (list-ref bodies pos)
                       env)))))
                (apply-env saved-env search-sym)))))))

(define list-index
  (lambda (sym los)
    (let loop ((pos 0) (los los))
      ;; los is at position pos of the original los
      (cond
        ((null? los) #f)
        ((eqv? sym (car los)) pos)
        (else (loop (+ pos 1) (cdr los)))))))

(define environment?
  (list-of
   (lambda (p)
     (and
      (pair? p)
      (or (eqv? (car p) 'let) (eqv? (car p) 'letrec))))))

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
   (proc proc?))
  (pair-val
   (car expval?)
   (cdr expval?)))

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
      (else (report-expval-extractor-error 'proc val)))))

(define-datatype proc proc?
  (procedure
   (vars (list-of identifier?))
   (body expression?)
   (env environment?)))

(define-datatype expression expression?
  (const-exp
   (num number?))
  (diff-exp
   (exp1 expression?)
   (exp2 expression?))
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

(define run
  (lambda (e)
    (value-of/k e (empty-env) (end-cont))))

(define exp1
  (let-exp 'f
           (proc-marg-exp '(x) (diff-exp (var-exp 'x) (const-exp 1)))
           (call-exp (var-exp 'f) (list (const-exp 3)))))

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
