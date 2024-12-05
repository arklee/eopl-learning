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

(define-datatype proc proc?
  (procedure
   (var identifier?)
   (body expression?)
   (saved-env environment?)))

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (proc-val
   (proc proc?))
  (mutex-val
   (mut mutex?)))

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

(define expval->mutex
  (lambda (val)
    (cases expval val
      (mutex-val (mut) mut)
      (else (report-expval-extractor-error 'mutex val)))))

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
     ("print" "(" expression ")")
     print-exp)

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
     ("spawn" "(" expression ")")
     spawn-exp)

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
  (lambda (timeslice pgm)
    (initialize-store!)
    (initialize-scheduler! timeslice)
    (cases program pgm
      (a-program (exp)
                 (value-of/k exp (empty-env) (end-main-thread-cont))))))

(define run
  (lambda (timeslice string)
    (value-of-program timeslice (scan&parse string))))

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
   (body expression?))
  (spawn-exp
   (exp expression?))
  (begin-exp
    (exp1 expression?)
    (exps (list-of expression?)))
  (print-exp
   (exp expression?))
  (mutex-exp)
  (wait-exp
   (exp expression?))
  (signal-exp
   (exp expression?))
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
      (zero?-exp (exp)
                 (value-of/k exp env (zero1-cont cont)))
      (var-exp (var) (apply-cont cont (deref (apply-env env var))))
      (let-exp
       (var exp1 body)
       ;; (value-of/k (call-exp (proc-exp var body) exp1) env cont)
       (value-of/k exp1 env (let-exp-cont var body env cont)))
      (if-exp
       (exp1 exp2 exp3)
       (value-of/k exp1 env (if-test-cont exp2 exp3 env cont)))
      (proc-exp
       (var body)
       (apply-cont cont (proc-val (procedure var body env))))
      (letrec-exp (var proc-var proc-body body)
                  (let* ([loc (newref (num-val 27))]
                         [new-env (extend-env var loc env)])
                    (begin
                      (setref! loc (proc-val (procedure proc-var proc-body new-env)))
                      (value-of/k body new-env cont))))
      ;; (begin-exp (exp exps)
      ;;            (if (null? exps)
      ;;                (value-of/k exp env cont)
      ;;                (value-of/k
      ;;                 (call-exp
      ;;                  (proc-exp
      ;;                   (fresh-identifier 'dummy)
      ;;                   (begin-exp (car exps) (cdr exps)))
      ;;                  exp)
      ;;                 env
      ;;                 cont)))
      (begin-exp (exp exps)
                 (if (null? exp)
                     (value-of/k exp env cont)
                     (value-of/k exp env (begin-cont exps env cont))))
      (spawn-exp (exp)
                 (value-of/k exp env (spawn-cont cont)))
      ;; (spawn-exp (exp)
      ;;            (place-on-ready-queue!
      ;;             (lambda ()
      ;;               (value-of/k exp env (end-subthread-cont cont))))
      ;;            (apply-cont cont (num-val 73)))
      (assign-exp (var exp)
                  (let ([loc (apply-env env var)])
                    (value-of/k exp env (set-rhs-cont loc cont))))
      (print-exp (exp)
                 (value-of/k exp env (print-cont cont)))
      (mutex-exp ()
                 (apply-cont cont (mutex-val (new-mutex))))
      (wait-exp (mut)
                (value-of/k mut env (wait-cont cont)))
      (signal-exp (mut)
                (value-of/k mut env (signal-cont cont)))
      (call-exp (rator rand)
                (value-of/k rator env (rator-cont rand env cont))))))

(define-datatype continuation continuation?
  (end-main-thread-cont)
  (end-subthread-cont)
  (zero1-cont
   (cont continuation?))
  (let-exp-cont
   (var identifier?)
   (body expression?)
   (env environment?)
   (cont continuation?))
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
  (begin-cont
    (exps (list-of expression?))
    (env environment?)
    (cont continuation?))
  (spawn-cont
   (cont continuation?))
  (print-cont
   (cont continuation?))
  (wait-cont
   (cont continuation?))
  (signal-cont
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
    (if (time-expired?)
        (begin
          (place-on-ready-queue!
           (lambda () (apply-cont cont val)))
          (run-next-thread))
        (begin
         (decrement-timer!)
         (cases continuation cont
           (end-main-thread-cont ()
                                 (set-final-answer! val)
                                 (run-next-thread))
           (end-subthread-cont ()
                               (run-next-thread))
           (zero1-cont (cont)
                       (apply-cont cont (bool-val (zero? (expval->num val)))))
           (let-exp-cont (var body env cont)
                         (value-of/k body (extend-env var (newref val) env) cont))
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
           (begin-cont (exps env cont)
                       (let ([first (car exps)]
                             [rest (cdr exps)])
                         (if (null? rest)
                             (value-of/k first env cont)
                             (value-of/k first env (begin-cont rest env cont)))))
           (spawn-cont (saved-cont)
                       (let ((proc1 (expval->proc val)))
                         (place-on-ready-queue!
                          (lambda ()
                            (apply-procedure/k proc1
                                               (num-val 28)
                                               (end-subthread-cont)))))
                       (apply-cont saved-cont (num-val 73)))
           (print-cont (cont)
                       (begin
                         (eopl:pretty-print val)
                         (apply-cont cont (num-val 26))))
           (wait-cont (saved-cont)
                      (wait-for-mutex
                       (expval->mutex val)
                       (lambda () (apply-cont saved-cont (num-val 52)))))
           (signal-cont (saved-cont)
                        (signal-mutex
                         (expval->mutex val)
                         (lambda () (apply-cont saved-cont (num-val 53)))))
           (rator-cont (rand env cont)
                       (value-of/k rand env (rand-cont val cont)))
           (rand-cont (val1 cont)
                      (let ((proc1 (expval->proc val1)))
                        (apply-procedure/k proc1 val cont))))))))

(define apply-procedure/k
  (lambda (proc1 val cont)
    (cases proc proc1
      (procedure
       (var body saved-env)
       (value-of/k body (extend-env var (newref val) saved-env) cont))
      (else (eopl:error 'call-exp "~s is not a procedure" proc1)))))

(define the-ready-queue 'uninitialized)
(define the-final-answer 'uninitialized)
(define the-max-time-slice 'uninitialized)
(define the-time-remaining 'uninitialized)

(define initialize-scheduler!
  (lambda (ticks)
    (set! the-ready-queue (empty-queue))
    (set! the-final-answer 'uninitialized)
    (set! the-max-time-slice ticks)
    (set! the-time-remaining the-max-time-slice)))

(define place-on-ready-queue!
  (lambda (th)
    (set! the-ready-queue
          (enqueue the-ready-queue th))))

(define run-next-thread
  (lambda ()
    (if (emtpy-queue? the-ready-queue)
        the-final-answer
        (dequeue
         the-ready-queue
         (lambda (th q)
           (set! the-ready-queue q)
           (set! the-time-remaining the-max-time-slice)
           (th))))))

(define set-final-answer!
  (lambda (val)
    (set! the-final-answer val)))

(define time-expired?
  (lambda ()
    (zero? the-time-remaining)))

(define decrement-timer!
  (lambda ()
    (set! the-time-remaining (- the-time-remaining 1))))

(define wait-for-mutex
  (lambda (m th)
    (cases mutex m
      (a-mutex (ref-to-closed? ref-to-wait-queue)
               (cond
                 ((deref ref-to-closed?)
                  (setref! ref-to-wait-queue
                           (enqueue (deref ref-to-wait-queue) th))
                  (run-next-thread))
                 (else
                  (setref! ref-to-closed? #t) (th)))))))

(define signal-mutex
  (lambda (m th)
    (cases mutex m
      (a-mutex (ref-to-closed? ref-to-wait-queue)
               (let ((closed? (deref ref-to-closed?))
                     (wait-queue (deref ref-to-wait-queue)))
                 (if closed?
                     (if (emtpy-queue? wait-queue)
                         (setref! ref-to-closed? #f)
                         (dequeue wait-queue
                                  (lambda (first-waiting-th other-waiting-ths)
                                    (place-on-ready-queue!
                                     first-waiting-th)
                                    (setref!
                                     ref-to-wait-queue
                                     other-waiting-ths))))
                     (th)))))))

(define empty-queue
  (lambda ()
    '()))

(define emtpy-queue? null?)

(define enqueue
  (lambda (q val)
    (append q (list val))))

(define dequeue
  (lambda (q f)
    (f (car q) (cdr q))))

(define-datatype mutex mutex?
  (a-mutex
   (ref-to-closed? reference?)
   (ref-to-wait-queue reference?)))

(define new-mutex
  (lambda ()
    (a-mutex
     (newref #f)
     (newref '()))))

(define prog1
  "letrec f(n)
          = if zero?(n)
               then 1
               else *(n, (f -(n, 1)))
   in (f 5)")

(define prog2
  "let x = 5 in
   let prod = 1 in
       letrec f(n) =
              begin
                set prod = *(prod, x);
                print(prod);
                set x = -(x, 1);
                if zero?(x)
                then prod
                else (f 27)
              end
        in (f 27)")

(define prog3
  "let n = 10 in
   let a = 1 in
   let b = 1 in
   letrec pa(n) =
            begin
              print (a);
              set n = -(n, 1);
              set a = *(a, 2);
              if zero?(n)
              then 280
              else (pa n)
            end in
   letrec pb(n) =
            begin
              print (b);
              set n = -(n, 1);
              set b = *(b, 3);
              if zero?(n)
              then 270
              else (pb n)
            end in
   begin
     spawn (proc (d) (pa n));
     spawn (proc (d) (pb n))
   end")

;(run 28 prog3)