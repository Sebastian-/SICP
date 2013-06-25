#lang planet neil/sicp

;; Exercise 1.45

(define (repeated f n)
  (define (compose f g)
    (lambda (x) (f (g x))))
  (define (loop f-n n)
    (if (<= n 1)
        f-n
        (loop (compose f f-n) (- n 1))))
  (loop f n))

(define (average-damp f)
  (define (average a b)
    (/ (+ a b) 2))
  (lambda (x) (average x (f x))))

(define (fixed-point f first-guess)
  (define tolerance 0.0000001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; Here I varied the number argument to (repeated average-damp n) and ran the tests
;; below until they converged

(define (test-n-root x n)
  (fixed-point ((repeated average-damp 4) (lambda (y) (/ x (expt y (- n 1))))) 1.0))

(test-n-root 256 2)
(test-n-root 256 3)
(test-n-root 256 4) ;; Requires 2 average damps
(test-n-root 256 5)
(test-n-root 256 6)
(test-n-root 256 7)
(test-n-root 256 8) ;; Requires 3 average damps
(test-n-root 256 9)
(test-n-root 256 10)
(test-n-root 256 11)
(test-n-root 256 12)
(test-n-root 256 13)
(test-n-root 256 14)
(test-n-root 256 15)
(test-n-root 256 16) ;; Requires 4 average damps

;; Based on the results above we should repeat average-damp floor(log_2(n)) times

(define (nth-root x n)
  (define (num-of-damps n)
    (define (loop n count)
      (if (< n 2)
          count
          (loop (/ n 2) (+ count 1))))
    (loop n 0))
  (fixed-point ((repeated average-damp (num-of-damps n)) (lambda (y) (/ x (expt y (- n 1))))) 
               1.0))

(nth-root 256 2)
(nth-root 256 3)
(nth-root 256 4)
(nth-root 256 5)
(nth-root 256 6)
(nth-root 256 7)
(nth-root 256 8)
(nth-root 256 9)
(nth-root 256 10)
(nth-root 256 11)
(nth-root 256 12)
(nth-root 256 13)
(nth-root 256 14)
(nth-root 256 15)
(nth-root 256 16)
