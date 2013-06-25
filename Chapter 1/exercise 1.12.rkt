#lang planet neil/sicp

;; Exercise 1.12

(define (pascal row col)
    (cond ((or (< row 0) (< col 0)) 0)
          ((= col 0) 1)
          ((= col row) 1)
          (else (+ (pascal (- row 1) (- col 1)) (pascal (- row 1) col)))))