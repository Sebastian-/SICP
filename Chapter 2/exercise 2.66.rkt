#lang racket

;; Exercise 2.66


;; Tree representation
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

;; binary tree lookup
(define (lookup search-key records)
  (cond [(null? records) false]
        [(equal? search-key (key (entry record)))
         (entry record)]
        [(less-than? search-key (key (entry record)))
         (lookup search-key (left-branch record))]
        [(greater-than? search-key (key (entry record)))
         (lookup search-key (right-branch record))]))