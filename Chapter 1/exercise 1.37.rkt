#lang planet neil/sicp

;; Exercise 1.37

;; Part a ======================================================================
;; Recursive Implementation

(define (cont-frac n d k)
  (define (recurs count)
    (if (> count k)
        0
        (/ (n count) (+ (d count) (recurs (+ count 1))))))
  (recurs 1))

;; 1/golden ratio = 0.6180339...

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           1)
;; 1.0

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           5)
;; 0.625

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           10)
;; 0.6179775...

;; Part b ======================================================================
;; Iterative Implementation

(define (iter-cont-frac n d k)
  (define (iter k total)
    (if (= k 0)
        total
        (iter (- k 1) (/ (n k) (+ (d k) total)))))
  (iter k 0))

(iter-cont-frac (lambda (i) 1.0)
                (lambda (i) 1.0)
                1)

(iter-cont-frac (lambda (i) 1.0)
                (lambda (i) 1.0)
                5)

(iter-cont-frac (lambda (i) 1.0)
                (lambda (i) 1.0)
                10)