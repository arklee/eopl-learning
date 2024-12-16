#lang eopl

(define identifier? symbol?)

(define vname
  (let ((n 0))
    (lambda ()
      (set! n (+ n 1))
      (string->symbol (string-append "v" (number->string n))))))

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
   (vars (list-of identifier?))
   (body expression?))
  (call-exp
   (rator expression?)
   (rands (list-of expression?)))
  (letrec-exp
   (var identifier?)
   (proc-var identifier?)
   (proc-body expression?)
   (body expression?)))

(define cps-of/k
  (lambda (exp cont)
    (cases expression exp
      (var-exp (var) (apply-cont cont exp))
      (const-exp (num) (apply-cont cont exp))
      (diff-exp (arg1 arg2)
                (cps-of/k arg1 (diff1-cont arg2 cont)))
      (call-exp (rator rands)
                (cps-of/k rator (rator-cont rands cont)))
      (else 'error))))

(define-datatype continuation continuation?
  (end-cont)
  (diff1-cont
   (arg2 expression?)
   (cont continuation?))
  (diff2-cont
   (val1 expression?)
   (cont continuation?))
  (rator-cont
   (rands (list-of expression?))
   (cont continuation?))
  (rands-cont
   (val1 (or expression? (equal? 'dummy)))
   (rest (list-of expression?))
   (cont continuation?)))

(define apply-cont
  (lambda (cont val)
    (cases continuation cont
      (end-cont ()
                (eopl:printf "End of Continuation\n")
                (call-exp (var-exp 'k) (list val)))
      (diff1-cont (arg2 saved-cont)
                  (cps-of/k arg2 (diff2-cont val saved-cont)))
      (diff2-cont (val1 saved-cont)
                  (apply-cont saved-cont (diff-exp val1 val)))
      (rator-cont (rands saved-cont)
                  (if (null? rands)
                      (apply-cont (rands-cont val '() saved-cont) 'dummy)
                      (cps-of/k (car rands) (rands-cont val (cdr rands) saved-cont))))
      (rands-cont (val1 rest saved-cont)
                  (cases continuation saved-cont
                    (end-cont ()
                              (eopl:printf "End of Continuation\n")
                              (call-exp val1 (list val (var-exp 'k))))
                    (else (let ((vn (vname)))
                            (call-exp val1 (list val (proc-exp (list vn) (apply-cont saved-cont (var-exp vn)))))))))
      (else 'error))))

(define cps
  (lambda (e)
    (cps-of/k e (end-cont))))

(define exp1
  (call-exp (var-exp 'f) (list (const-exp 2))))

(define exp2
  (call-exp (var-exp 'g) (list (call-exp (var-exp 'f) (list (var-exp 'x))))))

(define exp3
  (call-exp (var-exp 'h) (list (call-exp (var-exp 'g) (list (call-exp (var-exp 'f) (list (var-exp 'x))))))))

(define exp4
  (diff-exp (call-exp (var-exp 'g) (list (const-exp 3))) (call-exp (var-exp 'f) (list (var-exp 'x)))))

(eopl:pretty-print (cps exp3))