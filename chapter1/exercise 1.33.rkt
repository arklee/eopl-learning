#lang eopl

(define-datatype bintree-serch-tree bintree-serch-tree?
  (empty-bst)
  (node-bst
   (num number?)
   (left bintree-serch-tree?)
   (right bintree-serch-tree?)))

(define bst1
  (node-bst 14
            (node-bst 7 (empty-bst) (node-bst 12 (empty-bst) (empty-bst)))
            (node-bst 26
                      (node-bst 20 (node-bst 17 (empty-bst) (empty-bst))(empty-bst))
                      (node-bst 30 (empty-bst) (empty-bst)))))

(define path
  (lambda (n bst)
    (cases bintree-serch-tree bst
      (empty-bst () '(not-found))
      (node-bst
       (num left right)
       (cond [(= n num) '()]
             [(> n num) (cons 'right (path n right))]
             [else (cons 'left (path n left))])))))


  
                      