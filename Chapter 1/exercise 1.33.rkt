#lang planet neil/sicp

;; Exercise 1.33

(define (filtered-accumulate filter combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (if (filter a)
                    (term a)
                    null-value)
                (filtered-accumulate filter combiner null-value term (next a) next b))))

;; Part a ======================================================================

;; prime? implementation from exercise 1.28

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

(define (sum-of-sqr-primes a b)
  (define (filter a) (prime? a))
  (define (square x) (* x x))
  (define (inc x) (+ x 1))
  (filtered-accumulate filter + 0 square a inc b))

(sum-of-sqr-primes 2 6)
;; 2^2 + 3^2 + 5^2 = 38

(sum-of-sqr-primes 4 12)
;; 5^2 + 7^2 + 11^2 = 195

;; Part b ======================================================================

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (rel-prime-prod n)
  (define (relatively-prime? a)
    (= (gcd a n) 1))
  (define (identity x) x)
  (define (inc x) (+ x 1))
  (filtered-accumulate relatively-prime? * 1 identity 1 inc n))

(gcd 8 1)
(gcd 8 2)
(gcd 8 3)
(gcd 8 4)
(gcd 8 5)
(gcd 8 6)
(gcd 8 7)
(rel-prime-prod 8)
;; 1 x 3 x 5 x 7 = 105