#lang planet neil/sicp

;; Exercise 2.13

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

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define a (make-center-percent 100 1))
(define a-sqr (mul-interval a a))


(center a)
(percent a)
(center a-sqr)
(percent a-sqr)

;; Running the code above, it appears that when multiplying two numbers with small
;; percentage tolerances the resulting percentage tolerance is a sum of the two tolerances