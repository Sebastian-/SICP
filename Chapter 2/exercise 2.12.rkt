#lang planet neil/sicp

;; Exercise 2.12

(define (make-interval a b) (cons a b))

(define (upper-bound interval)
  (cdr interval))

(define (lower-bound interval)
  (car interval))

(define (make-center-percent center percent)
  (let ((lower-bound (* center (- 1 (/ percent 100.0))))
        (upper-bound (* center (+ 1 (/ percent 100.0)))))
    (make-interval lower-bound upper-bound)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (percent i)
  (* (- (/ (upper-bound i) (center i)) 1) 100))

(define i (make-center-percent 5 1))
(center i)
(percent i)