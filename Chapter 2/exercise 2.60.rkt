#lang racket

;; Exercise 2.60

;; element-of-set and intersection-set remain unchanged
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;; Since the underlying list can contain duplicates, any new element can
;; be adjoined without running any checks
(define (adjoin-set x set)
  (cons x set))

;; Similarly for union-set
(define (union-set s1 s2)
  (append s1 s2))

;; Both union/adjoin-set are faster as they dont need to call 
;; element-of-set anymore. This makes building up sets much faster than
;; the non-duplicate counterparts. adjoin-set becomes O(1) and union-set 
;; O(n). In contrast, we can expect element-of-set and intersection-set 
;; to be slower depending on how much duplication is in the list sets. The
;; duplicate representation may be preferrable in an application where sets
;; are often being constructed or joined together, but not searched or 
;; intersected very often.


