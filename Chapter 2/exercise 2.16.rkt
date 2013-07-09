#lang racket

;; Exercise 2.16

;; Data abstraction ====================

(define (make-interval a b) (cons a b))

(define (upper-bound interval)
  (cdr interval))

(define (lower-bound interval)
  (car interval))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (percent i)
  (* (- (/ (upper-bound i) (center i)) 1) 100))

;; Procedures ==========================================================

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


;; Equivalent algebraic expressions lead to different answers when using 
;; interval-arithmetic because each operation using an inexact interval will introduce
;; some additional error. This in turn biases the result in some direction. Constructing 
;; equivalent expressions with inexact intervals is fairly striaghtforward. Consider the 
;; exact interval:

(define one (make-interval 1 1))

;; and the inexact interval:

(define hundred (make-interval 99 101))

;; The following are all equivalent algebraic expressions, yet each has a different value
;; because of the number of times the inexact interval is used.

(define one1 one)
(define one2 
  (mul-interval one1 
                (div-interval hundred hundred)))
(define one3
  (mul-interval one2
                (div-interval hundred hundred)))

(center one1)
(percent one1)
(center one2)
(percent one2)
(center one3)
(percent one3)

;; Results =============================================================

;; 1
;; 0
;; 1.0002000200020003
;; 1.9998000199979993
;; 1.0008001600240033
;; 3.9980011593244047

;; Had to look up the answer to the second part of this question. According to wikipedia, 
;; in general it is impossible for an interval-arithmetic package to provide the same 
;; answer to equivalent expressions. More can be found at:
;;
;; http://en.wikipedia.org/wiki/Interval_arithmetic#Dependency_problem
