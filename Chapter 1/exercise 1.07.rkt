#lang planet neil/sicp

;; Exercise 1.7

;;(define (good-enough? guess x)
;;  (< (abs (- (square guess) x)) 0.001))

;; This good-enough test is not adequate for very small numbers because after a certain
;; point, the allowed error will be very large in comparison to the value of the square 
;; root. This results in an inaccurate answer. In the case of large numbers, due to the 
;; limited precision of a computer, a difference of 0.001 may be indistiguishable from a
;; difference of 0.01, or 0.1, or 1, etc. In such a case, this test won't guarantee 
;; anything about the accuracy of the result. Some solutions have said that in this case
;; the algorithm will not terminate, but that's not true. Eventually the computer won't be
;; able to tell the difference between (square guess) and x, so their difference will be 0
;; and the test will be satisfied. The only problem is that our guess is not necessarily
;; going to be accurate to the 3rd decimal, as the test suggests.

;; Improved square root procedure

(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? prev-guess guess)
  (<= (abs (- prev-guess guess)) (* guess 0.001)))

(define (sqrt-iter prev-guess guess x)
  (if (good-enough? prev-guess guess)
      guess
      (sqrt-iter guess (improve guess x)
                 x)))

(define (sqrt x)
  (sqrt-iter 0.0 1.0 x))

;; This test works better for both smaller and larger numbers. Note the use of <= in the
;; good-enough? test. This is required to ensure that the algorithm terminates when 
;; dealing with very small numbers. For small enough numbers, the computer will not be
;; able to distinguish between them, so a test using < will never be true (because to the
;; computer, the numbers are equal). To see this, use the < operator in good-enough? and 
;; call (sqrt 0). The algorithm will not terminate.