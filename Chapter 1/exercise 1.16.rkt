#lang planet neil/sicp

;; Exercise 1.16

(define (square x)
  (* x x))

(define (even? n)
(= (remainder n 2) 0))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (expt b n)
  (define (fast-expt-iter b n a)
    (cond [(= n 0) a]
          [(even? n) (fast-expt-iter (square b) (/ n 2) a)]
          [else (fast-expt-iter b (- n 1) (* a b))]))
  (fast-expt-iter b n 1))