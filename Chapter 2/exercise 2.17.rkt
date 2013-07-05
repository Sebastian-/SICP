#lang planet neil/sicp

;; Exercise 2.17

(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))

(last-pair (list 23 72 149 34))
      