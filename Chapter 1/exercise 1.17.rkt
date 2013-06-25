#lang planet neil/sicp

;; Exercise 1.17

(define (double x)
  (+ x x))

(define (halve x)
  (/ x 2))

(define (even? n)
  (= (remainder n 2) 0))

(define (* a b)
  (cond [(= b 1) a]
        [(even? b) (* (double a) (halve b))]
        [else (+ a (* a ( - b 1)))]))