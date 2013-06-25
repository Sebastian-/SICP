#lang planet neil/sicp

;; Exercise 1.42

(define (inc x)
  (+ 1 x))

(define (square x)
  (* x x))

(define (compose f g)
  (lambda (x) (f (g x))))

((compose square inc) 6)