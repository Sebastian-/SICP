#lang racket

;; Exercise 2.80

(define (=zero? x) (apply-generic 'equ? x))

(define (=zero?-scheme-number x)
  (= (contents x) 0))
(put '=zero? '(scheme-number) =zero?-scheme-number)

(define (=zero?-rational x)
  (= (numer x) 0))
(put '=zero? '(rational) =zero?-rational)

(define (=zero?-complex x)
  (= (magnitude x) 0))
(put '=zero? '(complex) =zero?-complex)
