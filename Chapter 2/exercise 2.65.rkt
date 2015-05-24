#lang racket

;; Exercise 2.65

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch tree)
                             result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts
                     (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

;; Union

;; Composition of 3 O(n) functions: tree->list, union-list, list->tree

;; union-set from exercise 2.62
(define (union-list s1 s2)
  (cond [(null? s1) s2]
        [(null? s2) s1]
        [else (let [(x1 (car s1))
                    (x2 (car s2))]
                (cond [(= x1 x2) (cons x1 (union-set (cdr s1) (cdr s2)))]
                      [(> x1 x2) (cons x2 (union-set s1 (cdr s2)))]
                      [(< x1 x2) (cons x1 (union-set (cdr s1) s2))]))]))

(define (union-set s1 s2)
  (list->tree (union-list (tree->list s1)
                          (tree->list s2))))

;; Intersection

;; Like the union function, intersection is a composition of 3 O(n) 
;; functions, making it O(n) as well

;; intersection-set for the ordered list representation
(define (intersection-list set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set (cdr set1)
                                          (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

(define (intersection-set s1 s2)
  (list->tree (intersection-list (tree->list s1)
                                 (tree->list s2))))