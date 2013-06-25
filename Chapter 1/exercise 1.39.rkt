#lang planet neil/sicp

;; Exercise 1.39

;; Notice that the continued fraction now has a difference rather than a sum in the
;; denominator!

(define (cont-frac n d k)
  (define (recurs count)
    (if (> count k)
        0
        (/ (n count) (- (d count) (recurs (+ count 1))))))
  (recurs 1))

(define (tan-cf x k)
  (define (N i)
    (if (= i 1)
        x
        (* x x)))
  (define (D i) (+ 1 (* 2 (- i 1))))
  (cont-frac N D k))

(tan-cf 0.5 1)
(tan-cf 0.5 5)
(tan-cf 0.5 10)