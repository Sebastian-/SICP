#lang racket


;; Exercise 2.61

(define (adjoin-set x set)
  (cond [(null? set) (cons x set)]
        [(= x (car set)) set]
        [(> x (car set)) (cons (car set) (adjoin-set x (cdr set)))]
        [(< x (car set)) (cons x set)]))

;; Testing

(define s1 (list 1 3 5 7))
(adjoin-set 0 '())
(adjoin-set 0 s1)
(adjoin-set 1 s1)
(adjoin-set 2 s1)
(adjoin-set 4 s1)
(adjoin-set 8 s1)