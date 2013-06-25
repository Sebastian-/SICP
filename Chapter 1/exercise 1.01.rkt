#lang planet neil/sicp

;; Exercise 1.1

;; By simply typing the statements in the interpreter and evaluating we get:

10
;; Evaluates to 10

(+ 5 3 4)
;; Evaluates to 12

(- 9 1)
;; Evaluates to 8

(/ 6 2) 
;; Evaluates to 3

(+ (* 2 4) (- 4 6))
;; Evaluates to 6

(define a 3)
;; No return value. The symbol 'a' is associated with '3' in the environment

(define b (+ a 1))
;; No return value. The symbol 'b' is associated with '4' in the environment

(+ a b (* a b))
;; Evaluates to 19

(= a b)
;; Evaluates to false

(if (and (> b a) (< b ( * a b)))
    b
    a)
;; Evaluates to 4

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
;; Evaluates to 16

(+ 2 (if (> b a) b a))
;; Evaluates to 6

(*(cond ((> a b) a)
        ((< a b) b)
        (else -1))
  (+ a 1))
;; Evaluates to 16