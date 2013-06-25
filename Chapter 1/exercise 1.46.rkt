#lang planet neil/sicp

;; Exercise 1.46

(define (iterative-improve good-enough? improve)
  (lambda (guess) (if (good-enough? guess)
                      guess
                      ((iterative-improve good-enough? improve) (improve guess)))))

(define (sqrt x)
  (define (square x)
    (* x x))
  (define (average x y)
    (/ (+ x y) 2))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  ((iterative-improve good-enough? improve) 1.0))

(define (fixed-point f)
  (define tolerance 0.00001)
  (define (close-enough? guess)
    (< (abs (- guess (f guess))) tolerance))
  (define (next guess)
    (f guess))
  ((iterative-improve close-enough? next) 1.0))