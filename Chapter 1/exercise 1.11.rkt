#lang planet neil/sicp

;; Exercise 1.11

;; Recursive

(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) (* 2 ( f (- n 2))) (* 3 (f (- n 3))))))

;; Iterative

(define (f-iter n)
  (define (iter-f fn fn-1 fn-2 fn-3 count)
    (if (>= count n)
        fn
        (iter-f (+ fn (* 2 fn-1) (* 3 fn-2)) fn fn-1 fn-2 (+ count 1))))
  (if (< n 3)
      n
      (iter-f 4 2 1 0 3)))
        
