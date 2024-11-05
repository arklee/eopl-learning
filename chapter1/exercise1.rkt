#lang eopl

(define duple
  (lambda (n x)
    (if (= n 0)
        '()
        (cons x (duple (- n 1) x)))))

;(display (duple 2 3))

(define invert
  (lambda (lst)
    (map (lambda (pair) (list (cadr pair) (car pair))) lst)))

;(display (invert '((a 1) (a 2) (1 b) (2 b))))

(define down
  (lambda (lst)
    (map (lambda (x) (list x)) lst)))

;(display (down '(a (more (complicated)) object)))

(define swapper
  (lambda (s1 s2 slist)
    (map (lambda (slist) (swapper-in-sexp s1 s2 slist)) slist)))

(define swapper-in-sexp
  (lambda (s1 s2 sexp)
    (if (symbol? sexp)
        (cond [(eqv? s1 sexp) s2]
              [(eqv? s2 sexp) s1]
              [else sexp])
        (swapper s1 s2 sexp))))

;(display (swapper 'x 'y '((x) y (z (x)))))

(define report-list-too-short
  (lambda (n)
    (eopl:error 'list-set
           "List too short by ~s elements.~%" (+ n 1))))

(define list-set
  (lambda (lst n x)
    (if (null? lst)
        (report-list-too-short n)
        (if (= n 0)
            (cons x (cdr lst))
            (cons (car lst) (list-set (cdr lst) (- n 1) x))))))

(define count-occurrences
  (lambda (s slist)
    (if (null? slist)
        0
        (+ (count-in-exp s (car slist)) (count-in-exp s (cdr slist))))))

(define count-in-exp
  (lambda (s sexp)
    (if (symbol? sexp)
        (if (eqv? sexp s) 1 0)
        (count-occurrences s sexp))))

(define combine-p
  (lambda (x sos)
    (if (null? sos)
        '()
        (cons (list x (car sos)) (combine-p x (cdr sos))))))
        
(define product
  (lambda (sos1 sos2)
    (map (lambda (x) (combine-p x sos2)) sos1)))

(define filter-in
  (lambda (pred lst)
    (cond [(null? lst) '()]
          [(pred (car lst)) (cons (car lst) (filter-in pred (cdr lst)))]
          [else (filter-in pred (cdr lst))])))

(define list-index-iter
  (lambda (pred lst n)
    (cond [(null? lst) #f]
          [(pred (car lst)) n]
          [else (list-index-iter pred (cdr lst) (+ n 1))])))

(define list-index
 (lambda (pred lst)
   (list-index-iter pred lst 0)))

(define every?
  (lambda (pred lst)
    (if (null? lst)
        #f
        (every?-not-null pred lst))))

(define every?-not-null
  (lambda (pred lst)
    (if (null? lst)
        #t
        (and (pred (car lst)) (every?-not-null pred (cdr lst))))))

(define exists?
  (lambda (pred lst)
    (if (null? lst)
        #f
        (or (pred (car lst)) (exists? pred (cdr lst))))))

(define up
  (lambda (lst)
    (cond [(null? lst) '()]
          [(symbol? (car lst)) (cons (car lst) (up (cdr lst)))]
          [else (append-alt (car lst) (up (cdr lst)))])))

(define append-alt
  (lambda (lst1 lst2)
    (if (null? lst1)
        lst2
        (cons (car lst1) (append-alt (cdr lst1) lst2)))))

(define flatten
  (lambda (slist)
    (cond [(null? slist) '()]
          [(symbol? (car slist)) (cons (car slist) (flatten (cdr slist)))]
          [else (append-alt (flatten (car slist)) (flatten (cdr slist)))])))

(define insert-int
  (lambda (n loi)
    (cond [(null? loi) (list n)]
          [(< n (car loi)) (cons n loi)]
          [else (cons (car loi) (insert-int n (cdr loi)))])))

(define merge
  (lambda (loi1 loi2)
    (if (null? loi1)
        loi2
        (insert-int (car loi1) (merge (cdr loi1) loi2)))))

(define merge2
  (lambda (loi1 loi2)
    (cond [(and (null? loi1) (not (null? loi2))) loi2]
          [(and (null? loi2) (not (null? loi1))) loi1]
          [else (let ([car1 (car loi1)]
                      [car2 (car loi2)])
                  (if (< car1 car2)
                      (cons car1 (merge2 (cdr loi1) loi2))
                      (cons car2 (merge2 loi1 (cdr loi2)))))])))

(define sort
  (lambda (lst)
    (if (null? lst)
        '()
        (insert-int (car lst) (sort (cdr lst))))))

(define insert/predicate
  (lambda (pred n loi)
    (cond [(null? loi) (list n)]
          [(pred n (car loi)) (cons n loi)]
          [else (cons (car loi) (insert/predicate pred n (cdr loi)))])))

(define sort/predicate
  (lambda (pred lst)
    (if (null? lst)
        '()
        (insert/predicate pred (car lst) (sort/predicate pred (cdr lst))))))
