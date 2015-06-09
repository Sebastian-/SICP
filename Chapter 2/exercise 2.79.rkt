#lang racket

;; Exercise 2.79

(define (equ? x y) (apply-generic 'equ? x y))

(define (equ?-scheme-number x y)
  (= (contents x) (contents y)))
(put 'equ? '(scheme-number scheme-number) equ?-scheme-number)

(define (equ?-rational x y)
  ;; this will work because rational numbers are always brought to
  ;; simplest terms since make-rat divides by the gcd
  (and (= (numer x) (numer y))
       (= (denom x) (denom y))))
(put 'equ? '(rational rational) equ?-rational)

(define (equ?-complex x y)
  (and (= (real-part x) (real-part y))
       (= (imag-part x) (imag-part y))))
(put 'equ? '(complex complex) equ?-complex)

;; An alternative to the implementation above, which is a little more
;; elegant, is to use the =zero? predicate in exercise 2.80 and the 
;; generic sub operation on x and y. This way equ? can simply be defined
;; as (=zero? (sub x y)) and will work for any algebraic expression.

