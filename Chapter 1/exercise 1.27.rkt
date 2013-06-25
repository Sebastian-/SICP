#lang planet neil/sicp

;; Exercise 1.27

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

(define (fermat-test n a)
    (= (expmod a n n) a))

(define (passes-fermat? n count)
  (cond [(>= count n) true]
        [(fermat-test n count) (passes-fermat? n (+ count 1))]
        [else false]))

(define (passes-all-fermat? n)
  (passes-fermat? n 1))

(passes-all-fermat? 561)
(passes-all-fermat? 1105)
(passes-all-fermat? 1729)
(passes-all-fermat? 2465)
(passes-all-fermat? 2821)
(passes-all-fermat? 6601)