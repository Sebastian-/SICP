#lang planet neil/sicp

;; Exercise 1.8

;; Using the improved version of good-enough? from exercise 1.7

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (improve guess x)
  (/ (+ (* 2 guess) (/ x (square guess))) 3))

(define (good-enough? prev-guess guess)
  (<= (abs (- prev-guess guess)) (* guess 0.001)))

(define (cubert-iter prev-guess guess x)
  (if (good-enough? prev-guess guess)
      guess
      (cubert-iter guess (improve guess x)
                 x)))

(define (cubert x)
  (cubert-iter 0.0 1.0 x))