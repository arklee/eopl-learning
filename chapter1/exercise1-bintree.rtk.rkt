#lang eopl

(define leaf
  (lambda (content)
    (list 'leaf content)))

(define interior-node
  (lambda (content lson rson)
    (list 'node content lson rson)))

(define leaf?
  (lambda (bintree)
    (eqv? 'leaf (car bintree))))

(define lson
  (lambda (bintree)
    (if (leaf? bintree)
        (eopl:error 'lson "~s is a leaf.~%" bintree)
        (caddr bintree))))

(define rson
  (lambda (bintree)
    (if (leaf? bintree)
        (eopl:error 'rson "~s is a leaf.~%" bintree)
        (cadddr bintree))))

(define contents-of
  (lambda (bintree)
    (cadr bintree)))

(define bintree1
  (interior-node 'red
                 (interior-node 'bar
                                (leaf 26)
                                (leaf 12))
                 (interior-node 'red
                                (leaf 11)
                                (interior-node 'quux
                                               (leaf 117)
                                               (leaf 14)))))

(define printtree
  (lambda (bintree)
    (if (leaf? bintree)
        (contents-of bintree)
        (list (contents-of bintree) (printtree (lson bintree)) (printtree (rson bintree))))))

(define double-tree
  (lambda (bintree)
    (if (leaf? bintree)
        (leaf (* 2 (contents-of bintree)))
        (interior-node (contents-of bintree)
                       (double-tree (lson bintree))
                       (double-tree (rson bintree))))))

(define mark-leaves-with-red-depth
  (lambda (bintree)
    (mlwrd-help bintree 0)))
    
(define mlwrd-help 
  (lambda (bintree rednum)
    (if  (leaf? bintree)
         (leaf rednum)
         (if (eqv? (contents-of bintree) 'red)
             (interior-node (contents-of bintree) (mlwrd-help (lson bintree) (+ 1 rednum)) (mlwrd-help  (rson bintree) (+ 1 rednum)))
             (interior-node (contents-of bintree) (mlwrd-help (lson bintree) rednum) (mlwrd-help  (rson bintree) rednum))))))

(define number-leaves-help
  (lambda (bintree n)
    (if (leaf? bintree)
        (cons (leaf n) 1)
        (let [(right (number-leaves-help (rson bintree) n))
              (left (number-leaves-help (lson bintree) n))]
          (cons (interior-node
                 (contents-of bintree)
                 (car left)
                 (car (number-leaves-help (rson bintree) (+ n (cdr left)))))
                (+ (cdr left) (cdr right)))))))

(define number-leaves
  (lambda (bintree)
    (number-leaves-help bintree 0)))

(display (printtree (car (number-leaves bintree1))))

