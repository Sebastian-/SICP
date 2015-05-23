#lang racket

;; Exercise 2.64

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  ;; base case, partial tree is empty, return an empty list.
  (if (= n 0)
      (cons '() elts)
      ;; Since balanced trees are being made, left subtree should have 
      ;; n/2 - 1 nodes (-1 for the root entry)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree elts left-size)))
          ;; (car left-result) is the left subtree
          (let ((left-tree (car left-result))
                ;; (cdr left-result) are the nodes used for the root and
                ;; right subtree
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            ;; this-entry is the root of the tree
            (let ((this-entry (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts
                     (cdr right-result)))
                ;; assembling the tree
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

;; Part a

;; partial-tree works by splitting each list it is passed into halves.
;; For each half it recursively produces the subtree of the half-list.
;; Eventually a list of length 1 is reached (i.e. a leaf) in which case
;; (partial-tree '(x) 1) is called, which reduces to (make-tree x '() '())
;; as we expect. Comments added to the code attempt to illustrate the 
;; process.

(list->tree '(1 3 5 7 9 11))

;       5
;     /   \
;    1     9
;     \   / \
;      3 7  11

;; Part b

;; Each entry in the list is visted once, prodcing 2 subtrees each. 
;; consing the the 3 elements in constant time suggests a 3n order of
;; growth or O(n).
