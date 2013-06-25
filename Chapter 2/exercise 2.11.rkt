#lang planet neil/sicp

;; Exercise 2.11

;; Intervals have only three possible sign configurations at the end points. Either both
;; are positive, both are negative, or the lower bound is negative and the upper bound is
;; positive. Thus, there are 3 x 3 = 9 possible endpoint cases.

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

(define (case-mul-interval x y)
  (define (pos-endpoints? interval)
    (and (>= (upper-bound interval) 0)
         (>= (lower-bound interval) 0)))
  (define (neg-endpoints? interval)
    (and (< (upper-bound interval) 0)
         (< (lower-bound interval) 0)))
  (define (mix-endpoints? interval)
    (and (>= (upper-bound interval) 0)
         (< (lower-bound interval) 0)))
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (cond [(and (pos-endpoints? x) (pos-endpoints? y)) 
           (make-interval p1 p4)]
          [(and (pos-endpoints? x) (mix-endpoints? y)) 
           (make-interval p3 p4)]
          [(and (pos-endpoints? x) (neg-endpoints? y)) 
           (make-interval p3 p2)]
          [(and (mix-endpoints? x) (pos-endpoints? y)) 
           (make-interval p2 p4)]
          [(and (mix-endpoints? x) (mix-endpoints? y)) 
           (make-interval (min p3 p2) (max p1 p4))]
          [(and (mix-endpoints? x) (neg-endpoints? y)) 
           (make-interval p3 p1)]
          [(and (neg-endpoints? x) (pos-endpoints? y)) 
           (make-interval p2 p3)]
          [(and (neg-endpoints? x) (mix-endpoints? y)) 
           (make-interval p2 p1)]
          [(and (neg-endpoints? x) (neg-endpoints? y)) 
           (make-interval p4 p1)])))

;; Testing

(define +i+ (make-interval 1 2))
(define -i+ (make-interval -1 2))
(define -i- (make-interval -2 -1))

;; pos x pos
(mul-interval +i+ +i+)
(case-mul-interval +i+ +i+)
;; pos x mix
(mul-interval +i+ -i+)
(case-mul-interval +i+ -i+)
;; pos x neg
(mul-interval +i+ -i-)
(case-mul-interval +i+ -i-)
;; mix x pos
(mul-interval -i+ +i+)
(case-mul-interval -i+ +i+)
;; mix x mix
(mul-interval -i+ -i+)
(case-mul-interval -i+ -i+)
;; mix x neg
(mul-interval -i+ -i-)
(case-mul-interval -i+ -i-)
;; neg x pos
(mul-interval -i- +i+)
(case-mul-interval -i- +i+)
;; neg x mix
(mul-interval -i- -i+)
(case-mul-interval -i- -i+)
;; neg x neg
(mul-interval -i- -i-)
(case-mul-interval -i- -i-)
