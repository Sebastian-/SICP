#lang planet neil/sicp

;; Exercise 2.30

(define (square-tree tree)
  (cond [(null? tree) nil]
        [(not (pair? tree)) (* tree tree)]
        [else (cons (square-tree (car tree))
                    (square-tree (cdr tree)))]))

(define (mapped-square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (mapped-square-tree sub-tree)
             (* sub-tree sub-tree)))
       tree))

;; Tests ===============================================================

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

(mapped-square-tree 
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

 (list 1
       (list 4 (list 9 16) 25)
       (list 36 49))