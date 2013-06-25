#lang planet neil/sicp

;; Exercise 1.24

;; Note that the 'random' function can only generate 32-bit integers, making
;; 0 - 4294967087 the largest range we can use.

;; Fermat test implementation ==================================================

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

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random 4294967087))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;; Primality testing framework =================================================

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (fast-prime? n 100))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display "***")
  (display elapsed-time))

(define (search-for-primes start end)
  (cond [(even? start) (search-for-primes (+ 1 start) end)]
        [(<= start end) (timed-prime-test start)
                        (search-for-primes (+ 2 start) end)]))

;; Display Output ==============================================================

(display "Exercise 1.24 - Using fast-prime")
(newline)
(display "Greater than 1e10")
(search-for-primes 10000000000 10000000061)
(newline)
(display "Greater than 1e11")
(search-for-primes 100000000000 100000000057)
(newline)
(display "Greater than 1e12")
(search-for-primes 1000000000000 1000000000063)
(newline)
(display "Greater than 1e20")
(search-for-primes 100000000000000000000 100000000000000000151)