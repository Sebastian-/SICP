#lang planet neil/sicp

;; Exercise 1.43

(define (square x)
  (* x x))

(define (repeated f n)
  (define (compose f g)
    (lambda (x) (f (g x))))
  (define (loop f-n n)
    (if (<= n 1)
        f-n
        (loop (compose f f-n) (- n 1))))
  (loop f n))

((repeated square 2) 5)