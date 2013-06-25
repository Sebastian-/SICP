#lang planet neil/sicp

;; Exercise 2.5

(define (divisible? x y)
  (= (remainder x y) 0))

(define (cons x y)
  (* (expt 2 x) (expt 3 y)))

(define (car p)
  (define (loop p count)
    (if (divisible? p 2)
        (loop (/ p 2) (+ count 1))
        count))
  (loop p 0))

(define (cdr p)
  (define (loop p count)
    (if (divisible? p 3)
        (loop (/ p 3) (+ count 1))
        count))
  (loop p 0))

(car (cons 4 5))
(cdr (cons 4 5))
(car (cons 0 1))
(cdr (cons 1 0))