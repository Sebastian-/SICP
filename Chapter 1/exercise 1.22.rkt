#lang planet neil/sicp

;; Exercise 1.22

(define (smallest-divisor n)
  (find-divisor n 2))

(define (square n)
  (* n n))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

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

;; Commented out ranges are recommended by the text, 
;; but computers are too fast for them nowadays

;(search-for-primes 1000 1019)
;(search-for-primes 10000 10037)
;(search-for-primes 100000 100043)
;(search-for-primes 1000000 1000037)

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
 
;; Running this program we see that running times roughly triple for every
;; increase of a multiple of 10. This is expected since we know that the growth
;; rate of this algorith is O(sqrt(n)) and sqrt(10) ~ 3.