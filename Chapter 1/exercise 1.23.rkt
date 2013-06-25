#lang planet neil/sicp

;; Exercise 1.23

(define (smallest-divisor n)
  (find-divisor n 2))

(define (square n)
  (* n n))

(define (next x)
  (if (= x 2)
      3
      (+ x 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display "***")
  (display elapsed-time))

(define (search-for-primes start end)
  (cond [(even? start) (search-for-primes (+ 1 start) end)]
        [(<= start end) (timed-prime-test start)
                        (search-for-primes (+ 2 start) end)]))

(display "Exercise 1.23 Optimization")
(newline)
(display "Greater than 10000000000")
(search-for-primes 10000000000 10000000061)
(newline)
(display "Greater than 100000000000")
(search-for-primes 100000000000 100000000057)
(newline)
(display "Greater than 1000000000000")
(search-for-primes 1000000000000 1000000000063)
(newline)
(display "Greater than 10000000000000")
(search-for-primes 10000000000000 10000000000099)

;; Although the number of test divisors is halved, we haven't saved
;; ourselves those steps for free. The next procedure requires a few
;; steps to check its predicate, and to add 2 to it's input in
;; most cases. Thus the improved algorithim will be less than twice as
;; fast as the original, but still faster.