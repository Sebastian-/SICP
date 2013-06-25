#lang planet neil/sicp

;; Exercise 1.40

(define (cubic a b c)
  (lambda (x) (+ (* x x x)
                 (* a x x)
                 (* b x)
                 c)))