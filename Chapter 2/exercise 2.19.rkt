#lang planet neil/sicp

;; Exercise 2.19

(define (no-more? l)
  (null? l))

(define (first-denomination l)
  (car l))

(define (except-first-denomination l)
  (cdr l))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define us-coins (list 50 25 10 5 1))
(define alt-us-coins (list 1 5 10 25 50))
(cc 100 us-coins)
(cc 100 alt-us-coins)

;; Results =============================================================

;; 292
;; 292

;; The order of the coin values in the lists does not affect the answer because it doesnt
;; matter what order you use coins to make change.