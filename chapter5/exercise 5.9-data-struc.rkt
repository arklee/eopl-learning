#lang eopl

(define identifier? symbol?)

(define-datatype environment environment?
  (empty-env)
  (extend-env
   (var identifier?)
   (loc integer?)
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
      (set! the-store (append the-store (list val)))
      next-ref)))

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
    (eopl:error 'setref! "No reference for ~s in ~s" ref the-store)))

(define fresh-identifier
  (let ((sn 0))
    (lambda (identifier)
      (set! sn (+ sn 1))
      (string->symbol
       (string-append
        (symbol->string identifier)
        "%"             ; this can't appear in an input identifier
        (number->string sn))))))

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

(define the-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier
       (letter (arbno (or letter digit "_" "-" "?")))
       symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))

(define the-grammar
  '((program (expression) a-program)

    (expression (number) const-exp)
    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)

    (expression
     ("*" "(" expression "," expression ")")
     mult-exp)

    (expression
     ("zero?" "(" expression ")")
     zero?-exp)

    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)

    (expression (identifier) var-exp)

    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)

    (expression
     ("proc" "(" identifier ")" expression)
     proc-exp)

    (expression
     ("set" identifier "=" expression)
     assign-exp)

    (expression
     ("begin" expression (arbno ";" expression) "end")
     begin-exp)

    (expression
     ("(" expression expression ")")
     call-exp)

    (expression
     ("letrec"
      identifier "(" identifier ")" "=" expression
      "in" expression)
     letrec-exp)

    ))

;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

;; (define show-the-datatypes
;;   (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

;; (define just-scan
;;   (sllgen:make-string-scanner the-lexical-spec the-grammar))

(define-datatype program program?
  (a-program
   (exp1 expression?)))

(define value-of-program
  (lambda (pgm)
    (initialize-store!)
    (cases program pgm
      (a-program (exp)
                 (value-of/k exp (empty-env) (end-cont))))))

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

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
   (body expression?))
  (begin-exp
    (exp1 expression?)
    (exps (list-of expression?)))
  (assign-exp
   (var identifier?)
   (exp expression?)))

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
      (var-exp (var) (apply-cont cont (deref (apply-env env var))))
      (let-exp
       (var exp1 body)
       (value-of/k (call-exp (proc-exp var body) exp1) env cont)
       ;; (value-of/k exp1 env (let-exp-cont var body env cont))
       )
      (if-exp
       (exp1 exp2 exp3)
       (value-of/k exp1 env (if-test-cont exp2 exp3 env cont)))
      (proc-exp
       (var body)
       (apply-cont cont (proc-val var body env)))
      (letrec-exp (var proc-var proc-body body)
                  (let* ([loc (newref (num-val 27))]
                         [new-env (extend-env var loc env)])
                    (begin
                      (setref! loc (proc-val proc-var proc-body new-env))
                      (value-of/k body new-env cont))))
      (begin-exp (exp exps)           ; this one, too
                 (if (null? exps)
                     (value-of/k exp env cont)
                     (value-of/k
                      (call-exp
                       (proc-exp
                        (fresh-identifier 'dummy)
                        (begin-exp (car exps) (cdr exps)))
                       exp)
                      env
                      cont)))
      (assign-exp (var exp)
                  (let ([loc (apply-env env var)])
                    (value-of/k exp env (set-rhs-cont loc cont))))
      (call-exp (rator rand)
                (value-of/k rator env (rator-cont rand env cont))))))

(define begin-cont
  (lambda (rest env cont)
    (lambda (val)
      (if (null? rest)
          (apply-cont cont val)
          (value-of/k (car rest) env (begin-cont (cdr rest) env cont))))))

(define-datatype continuation continuation?
  (end-cont)
  (zero1-cont
   (cont continuation?))
  ;; (let-exp-cont
  ;;  (var identifier?)
  ;;  (body expression?)
  ;;  (env environment?)
  ;;  (cont continuation?))
  (if-test-cont
   (exp2 expression?)
   (exp3 expression?)
   (env environment?)
   (cont continuation?))
  (diff1-cont
   (exp2 expression?)
   (env environment?)
   (cont continuation?))
  (diff2-cont
   (val expval?)
   (cont continuation?))
  (mult1-cont
   (exp2 expression?)
   (env environment?)
   (cont continuation?))
  (mult2-cont
   (val expval?)
   (cont continuation?))
  (set-rhs-cont
   (loc integer?)
   (cont continuation?))
  (rator-cont
   (rand expression?)
   (env environment?)
   (cont continuation?))
  (rand-cont
   (proc1 expval?)
   (cont continuation?)))

(define apply-cont
  (lambda (cont val)
    (cases continuation cont
      (end-cont ()
                (begin (eopl:printf "End of continuation. ~%")
                       val))
      (zero1-cont (cont)
                  (apply-cont cont (bool-val (zero? (expval->num val)))))
      ;; (let-exp-cont (var body env cont)
      ;;               (value-of/k body (extend-env var (newref val) env) cont))
      (if-test-cont (exp2 exp3 env cont)
                    (if (expval->bool val)
                        (value-of/k exp2 env cont)
                        (value-of/k exp3 env cont)))
      (diff1-cont (exp2 env cont)
                  (value-of/k exp2 env (diff2-cont val cont)))
      (diff2-cont (val1 cont)
                  (let ([num1 (expval->num val1)]
                        [num2 (expval->num val)])
                    (apply-cont cont (num-val (- num1 num2)))))
      (mult1-cont (exp2 env cont)
                  (value-of/k exp2 env (mult2-cont val cont)))
      (mult2-cont (val1 cont)
                  (let ([num1 (expval->num val1)]
                        [num2 (expval->num val)])
                    (apply-cont cont (num-val (* num1 num2)))))
      (set-rhs-cont (loc cont)
                    (begin
                      (setref! loc val)
                      (apply-cont cont (num-val 27))))
      (rator-cont (rand env cont)
                  (value-of/k rand env (rand-cont val cont)))
      (rand-cont (proc1 cont)
                 (apply-procedure/k proc1 val cont)))))

(define apply-procedure/k
  (lambda (proc1 val cont)
    (cases expval proc1
      (proc-val
       (var body saved-env)
       (value-of/k body (extend-env var (newref val) saved-env) cont))
      (else (eopl:error 'call-exp "~s is not a procedure" proc1)))))

(define prog1
  "letrec f(n)
          = if zero?(n)
               then 1
               else *(n, (f -(n, 1)))
   in (f 5)")

(define prog2
  "let x = 0
   in let y = 0
      in begin
          set x = 7;
          set y = 5;
          set x = *(x, y);
          -(x, y)
         end")

(eopl:pretty-print (run prog1))
