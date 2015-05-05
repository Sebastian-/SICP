#lang planet neil/sicp

;; Exercise 2.48

;; The neil/sicp language includes a picture language which has a built in
;; vector representation. I'll be using that instead of my own definitions
;; to avoid clutter. 

;; start and end are vectors
(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define v1 (make-vect 1 1))
(define v2 (make-vect 2 2))

(define s1 (make-segment v1 v2))
(start-segment s1)
(end-segment s1)


