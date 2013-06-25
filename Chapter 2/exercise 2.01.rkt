#lang planet neil/sicp

;; Exercise 2.1

(define(make-rat numerator denominator)
  (cond [(and (< numerator 0) (< denominator 0)) (cons (- numerator) (- denominator))]
        [(< denominator 0) (cons (- numerator) (- denominator))]
        [else (cons numerator denominator)]))