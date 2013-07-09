#lang planet neil/sicp

;; Exercise 2.31

(define (square x)
  (* x x))

(define (tree-map procedure tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map procedure sub-tree)
             (procedure sub-tree)))
       tree))

(define (square-tree tree)
  (tree-map square tree))

;; Test ================================================================

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

(list 1
      (list 4 (list 9 16) 25)
      (list 36 49))