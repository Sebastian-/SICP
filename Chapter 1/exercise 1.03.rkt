#lang planet neil/sicp

;; Exercise 1.3 

(define (max-sum-of-squares a b c)
  (define (sum-of-squares x y)
    (+ (* x x) (* y y)))
  (cond ((and (>= a c) (>= b c)) (sum-of-squares a b))
        ((and (>= a b) (>= c b)) (sum-of-squares a c))
        ((and (>= b a) (>= c a)) (sum-of-squares b c))))