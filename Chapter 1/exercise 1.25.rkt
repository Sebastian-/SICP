#lang planet neil/sicp

;; Exercise 1.25

(define (square x)
  (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))


;; Running both variations of the algorithm yeilds the same result, so it seems like
;; either representation is correct. However, the alternate algorithm is prone to 
;; integer overflow for large enough inputs. The original algorithm pairs 'square'
;; operations with 'remainder' operations, which keeps the numbers small throughout 
;; the entire process. In contrast, the second operation computes the entire exponential
;; term first before applying the 'remainder' operation at the very end. This is very 
;; taxing on both memory and computing power as the numbers increase exponentially. 
;; Using this alternate version of expmod in exercise 1.24 is not practical as it simply
;; takes too long to square such huge numbers and subsequently compute the remainder term.