#lang planet neil/sicp

;; Exercise 2.46

;; Vector operations are provided by the picture language in the neil/sicp
;; package. Nevertheless we implement them here for anyone interested.

(define (make-vect x y)
  (cons x y))

(define (xcor-vect vector)
  (car vector))

(define (ycor-vect vector)
  (cdr vector))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect scalar vector)
  (make-vect (* (xcor-vect vector) scalar)
             (* (ycor-vect vector) scalar)))

;; Testing

(define (displayVector v)
  (display "(")
  (display (xcor-vect v))
  (display ",")
  (display (ycor-vect v))
  (display ")")
  (newline))

(define v1 (make-vect 1 1))
(define v2 (make-vect 2 3))

(displayVector (add-vect v1 v2))
(displayVector (sub-vect v1 v2))
(displayVector (scale-vect 4 v2))