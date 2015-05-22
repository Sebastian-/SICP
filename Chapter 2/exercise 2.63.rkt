#lang racket

;; Exercise 2.63

;; Tree Representation

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

;; tree->list functions

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1
                     (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch tree)
                             result-list)))))
  (copy-to-list tree '()))

;; Part a

;; Both procedures produce the same lists since they both traverse the
;; entire tree, and the left-branch and right-branch lists are combined in
;; the same order. Both procedures produce the list '(1 3 5 7 9 11) for 
;; every tree in Figure 2.16, as shown below.

;; Part b

;; The second procedure grows more slowly because it constructs the list
;; iteratively. In contrast, the first procedure must append the left 
;; subtree list to the right, which takes an extra n/2 steps for a 
;; balanced tree. Both are on the order of O(n) however, since they visit
;; each node once.

;; figure 2.16 - left tree
(define t1 (list 7 
                 (list 3 
                       (list 1 '() '()) 
                       (list 5 '() '())) 
                 (list 9 
                       '() 
                       (list 11 '() '()))))

;; center tree
(define t2 (list 3 
                 (list 1 '() '())
                 (list 7 
                       (list 5 '() '())
                       (list 9
                             '()
                             (list 11 '() '())))))
;; right tree
(define t3 (list 5
                 (list 3
                       (list 1 '() '())
                       '())
                 (list 9
                       (list 7 '() '())
                       (list 11 '() '()))))

(tree->list-1 t1)
(tree->list-2 t1)

(tree->list-1 t2)
(tree->list-2 t2)

(tree->list-1 t3)
(tree->list-2 t3)