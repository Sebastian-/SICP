#lang planet neil/sicp

;; Exercise 1.44

(define (average a b c)
  (/ (+ a b c) 3))

(define (smooth f)
  (let ((dx 0.00001)))
  (lambda (x) (average (f (- x dx))
                       (f x)
                       (f (+ x dx)))))

(define (repeated f n)
  (define (compose f g)
    (lambda (x) (f (g x))))
  (define (loop f-n n)
    (if (<= n 1)
        f-n
        (loop (compose f f-n) (- n 1))))
  (loop f n))

(define (n-fold-smooth f n)
  (repeated smooth n))