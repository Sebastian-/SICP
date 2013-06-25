#lang planet neil/sicp

;; Exercise 2.9

;; Data abstraction ====================

(define (make-interval a b) (cons a b))

(define (upper-bound interval)
  (cdr interval))

(define (lower-bound interval)
  (car interval))

;; Procedures ==========================

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval a b)
  (make-interval (- (lower-bound a) (upper-bound b))
                 (- (upper-bound a) (lower-bound b))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (width interval)
  (/ (- (upper-bound interval) (lower-bound interval)) 2))

;; Examples

(define int1 (make-interval 3 4))
(define int2 (make-interval 5 10))
(define int3 (make-interval 10 15))
(width int1)
(width int2)
(width (add-interval int1 int2))
(width (sub-interval int1 int2))

(width (div-interval int1 int2))
(width (div-interval int1 int3))
(width (mul-interval int1 int2))
(width (mul-interval int1 int3))


;; Running the examples we see that the width of the sum or difference of two intervals
;; is the sum of their individual widths. However, for multiplication and division this
;; not the case. Notice that both int2 and int3 have the same width, but when they are
;; combined with int1 they yeild different widths. This suggests that the width of the
;; result of such operations is dependant not only on the width of the intervals, but
;; on the intervals themselves.