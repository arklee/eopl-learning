#lang eopl

(define identifier?
  (lambda (x)
    (and symbol? (not (eqv? x 'lambda)))))

(define-datatype env env?
  (empty-env)
  (extend-env
   (bound-var identifier?)
   (bound-val number?)
   (bound-env env?)))

(define has-binding?
  (lambda (search-var search-env)
    (cases env search-env
      (empty-env () #f)
      (extend-env (bound-var bound-val bound-env)
                  (if (eqv? search-var bound-var)
                      #t
                      (has-binding? search-var bound-env))))))

(define-datatype stack stack?
  (empty-stack)
  (push-to-stack
   (value (or symbol? number?))
   (bound-stack stack?)))

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
       (list 'interior-node key (bintree-to-list left) (bintree-to-list right))))))

(define tree-1
  (interior-node 'foo (leaf-node 2) (leaf-node 3)))

(define tree-2
  (interior-node 'bar (leaf-node -1) tree-1))

(define tree-3
  (interior-node 'baz tree-2 (leaf-node 1)))











