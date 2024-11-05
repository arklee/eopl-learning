#lang eopl

(define zero (lambda () '(diff (one) (one))))

(define predecessor
  (lambda (diff-tree)
    (list 'diff diff-tree '(one))))

(define sucessor
  (lambda (diff-tree)
    (list 'diff diff-tree '(diff (diff (one) (one)) (one)))))
