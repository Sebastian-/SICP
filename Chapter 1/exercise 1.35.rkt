#lang planet neil/sicp

;; Exercise 1.35

;; Showing that the golden ratio is a fixed point of the function f(x) = 1 + 1/x

;; 1 + 1/(golden ratio) = 1 + 2/(1 + sqrt(5))
;;                      = (3 + sqrt(5))/(1 + sqrt(5)
;;                      = (3 + sqrt(5))(1 - sqrt(5))/-4
;;                      = (-2 - 2(sqrt(5)))/-4
;;                      = (1 + sqrt(5))/2
;;                      = golden ratio

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define golden-ratio 
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

golden-ratio