#lang planet neil/sicp

;; Exercise 1.28

(define (square x n)
  (if (is-sqrt1-modn? x n)
      0
      (* x x)))

(define (miller-rabin-condition? x n)
  (= 1 (remainder (* x x) n)))

(define (is-sqrt1-modn? x n)
  (and (not (or (= x 1) (= x (- n 1))))
       (miller-rabin-condition? x n)))
      

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m) m)
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (prime? n)
  (fast-prime? n 100))

;; Tests =======================================================================

;; Not prime

(prime? 4)
(prime? 963)
(prime? 1000)
(prime? 2525)
(prime? 9)

;; Prime

(prime? 2083)
(prime? 3631)
(prime? 4721)
(prime? 6011)
(prime? 2)

;; Carmichael numbers

(prime? 561)
(prime? 1105)
(prime? 1729)
(prime? 2465)
(prime? 2821)
(prime? 6601)