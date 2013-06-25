#lang planet neil/sicp

;; Exercise 1.38

(define (divisible? x y)
  (= (remainder x y) 0))

(define (N i) 1.0)
(define (D i)
  (cond [(= i 1) i]
        [(divisible? i 3) 1]
        [(divisible? i 4) 1]
        [else (/ (+ (* 2 i) 2) 3)]))

(define (cont-frac n d k)
  (define (iter k total)
    (if (= k 0)
        total
        (iter (- k 1) (/ (n k) (+ (d k) total)))))
  (iter k 0))

(cont-frac N D 1)

(cont-frac N D 5)

(cont-frac N D 10)