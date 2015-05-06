#lang racket

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

;; Exercise 2.49

;; Notice the lang and require statements above. Currently the neil/sicp
;; language is not compatible with the picture language implementation
;; for this exercise.

;; Part a

;; Co-ordinates of vectors are given with respect to the unit square. 0.99
;; is used instead of 1 so that the top and right edges will display
(define v1 (make-vect 0 0))
(define v2 (make-vect 0.99 0))
(define v3 (make-vect 0 0.99))
(define v4 (make-vect 0.99 0.99))

(define edge1 (make-segment v1 v2))
(define edge2 (make-segment v1 v3))
(define edge3 (make-segment v2 v4))
(define edge4 (make-segment v3 v4))

(define outline (list edge1 edge2 edge3 edge4))

(paint (segments->painter outline))

;; Part b

(define edge/ (make-segment v1 v4))
(define edge\ (make-segment v2 v3))

(define X (list edge/ edge\ ))

(paint (segments->painter X))

;; Part c

(define v5 (make-vect 0 0.5))
(define v6 (make-vect 0.5 0))
(define v7 (make-vect 1 0.5))
(define v8 (make-vect 0.5 1))

(define edge5 (make-segment v5 v8))
(define edge6 (make-segment v5 v6))
(define edge7 (make-segment v8 v7))
(define edge8 (make-segment v6 v7))

(define diamond (list edge5 edge6 edge7 edge8))

(paint (segments->painter diamond))

;; Part d

(define wave-segments
  (list
   (make-segment
    (make-vect 0.416 0.999)
    (make-vect 0.333 0.833))
   (make-segment
    (make-vect 0.583 0.999)
    (make-vect 0.666 0.833))
   (make-segment
    (make-vect 0.666 0.833)
    (make-vect 0.583 0.666))
   (make-segment
    (make-vect 0.333 0.833)
    (make-vect 0.416 0.666))
   (make-segment
    (make-vect 0.416 0.666)
    (make-vect 0.333 0.666))
   (make-segment
    (make-vect 0.333 0.666)
    (make-vect 0.166 0.5))
   (make-segment
    (make-vect 0.166 0.5)
    (make-vect 0 0.666))
   (make-segment
    (make-vect 0.583 0.666)
    (make-vect 0.833 0.666))
   (make-segment
    (make-vect 0.833 0.666)
    (make-vect 0.999 0.5))
   (make-segment
    (make-vect 0.5 0.333)
    (make-vect 0.333 0))
   (make-segment
    (make-vect 0.5 0.333)
    (make-vect 0.666 0))
   (make-segment
    (make-vect 0.333 0.5)
    (make-vect 0.166 0))
   (make-segment
    (make-vect 0.333 0.5)
    (make-vect 0.166 0.333))
   (make-segment
    (make-vect 0.166 0.333)
    (make-vect 0 0.5))
   (make-segment
    (make-vect 0.666 0.5)
    (make-vect 0.833 0.5))
   (make-segment
    (make-vect 0.666 0.5)
    (make-vect 0.833 0))
   (make-segment
    (make-vect 0.833 0.5)
    (make-vect 0.999 0.333))))


(paint (segments->painter wave-segments))

