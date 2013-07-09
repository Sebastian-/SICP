#lang planet neil/sicp

;; Exercise 2.10

;; The problem with dividing by an interval which spans zero is that its reciprocal 
;; interval is not well defined.

; (define (div-interval x y)
;   (mul-interval x
;                 (make-interval (/ 1.0 (upper-bound y))
;                                (/ 1.0 (lower-bound y)))))

;; Notice that if the interval y spans 0, then it's lower bound will be negative. But in
;; making the reciprocal interval we set the upper bound to be one divided by the
;; (negative) lower bound. So the reciprocal interval will have a positive lower bound, 
;; but a negative upper bound. An interval like that doesn't make much sense!

(define (make-interval a b) (cons a b))

(define (upper-bound interval)
  (cdr interval))

(define (lower-bound interval)
  (car interval))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (let ((y-lower (lower-bound y))
        (y-upper (upper-bound y)))
    (if (and (<= y-lower 0) (> y-upper 0))
        (error "attempting to divide by an interval which spans 0 - " y)
        (mul-interval x
                      (make-interval (/ 1.0 (upper-bound y))
                                     (/ 1.0 (lower-bound y)))))))