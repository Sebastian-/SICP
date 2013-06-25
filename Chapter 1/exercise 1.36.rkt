#lang planet neil/sicp

;; Exercise 1.36

(define tolerance 0.00001)

(define (report-value n)
  (newline)
  (display n))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (report-value n)
    (newline)
    (display n))
  (define (try guess)
    (let ((next (f guess)))
      (cond [(close-enough? guess next) (report-value guess) (report-value next) next]
            [else (report-value guess) (try next)])))
  (try first-guess))

(define x
  (fixed-point (lambda (x) (/ (log 1000) (log x))) 2))

(define (average x y)
  (/ (+ x y) 2))

(define x-average-damping
  (fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2))

;; Number of steps used in each approach: 
;;     With average damping - 10
;;     Without average damping - 35