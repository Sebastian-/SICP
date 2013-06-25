#lang planet neil/sicp

;; Exercise 1.41

(define (inc x)
  (+ 1 x))

(define (double f)
  (lambda (x) (f (f x))))

(((double (double double)) inc) 5)