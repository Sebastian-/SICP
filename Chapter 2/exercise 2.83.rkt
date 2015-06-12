#lang racket

;; Exercise 2.83

(define (raise-real x)
  (make-complex-from-real-imag x 0))
(put 'raise '(real) raise-real)

(define (raise-rational x)
  (div (numer x) (denom x)))
(put 'raise '(rational) raise-rational)

(define (raise-integer x)
  (make-rational x 1))
(put 'raise '(integer) raise-integer)

(define (raise x)
  (apply-generic 'raise x))