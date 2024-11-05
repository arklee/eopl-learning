#lang eopl

(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))

(define bintree-to-list
  (lambda (tree)
    (cases bintree tree
      (leaf-node (num) (list 'leaf-node num))
      (interior-node
       (key left right)
       (list 'interior-node
             key
             (bintree-to-list left)
             (bintree-to-list right))))))

;(display (bintree-to-list (interior-node 'a (leaf-node 3) (leaf-node 4))))

(define tree-1
  (interior-node 'foo (leaf-node 2) (leaf-node 3)))

(define tree-2
  (interior-node 'bar (leaf-node -1) tree-1))

(define tree-3
  (interior-node 'baz tree-2 (leaf-node 1)))

(define mi-help
  (lambda (tree)
    (cases bintree tree
      (leaf-node (num) (list 'leaf num num))
      (interior-node
       (key left right)
       (let* [(left-list (mi-help left))
             (right-list (mi-help right))
             (sum (+ (caddr left-list) (caddr right-list)))]
         (if (> (cadr left-list) (cadr right-list))
             (if (> (cadr left-list) sum)
                 (list (car left-list) (cadr left-list) sum)
                 (list key sum sum))
             (if (> (cadr right-list) sum)
                 (list (car right-list) (cadr right-list) sum)
                 (list key sum sum))))))))

(define max-interior
  (lambda (tree)
    (car (mi-help tree))))


             
       