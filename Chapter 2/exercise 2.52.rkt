#lang racket

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

;; Exercise 2.52

;; Part a

;; To add a smile, simply include the extra line segments in the original
;; wave definition
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
    (make-vect 0.999 0.333))
   ;; Smile segments
   (make-segment
    (make-vect 0.444 0.95)
    (make-vect 0.444 0.9))
   (make-segment
    (make-vect 0.544 0.95)
    (make-vect 0.544 0.9))
   (make-segment
    (make-vect 0.5 0.75)
    (make-vect 0.444 0.8))
   (make-segment
    (make-vect 0.5 0.75)
    (make-vect 0.544 0.8))))

(paint (segments->painter wave-segments))

;; Part b

;; Changes to corner split can be done without changing right/up-split
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((top-left (up-split painter (- n 1)))
            (bottom-right (right-split painter (- n 1)))
            (corner (corner-split painter (- n 1))))
        (beside (below painter top-left)
                (below bottom-right corner)))))

;; Original definition

;(define (corner-split painter n)
;  (if (= n 0)
;      painter
;      (let ((up (up-split painter (- n 1)))
;            (right (right-split painter (- n 1))))
;        (let ((top-left (beside up up))
;              (bottom-right (below right right))
;              (corner (corner-split painter (- n 1))))
;          (beside (below painter top-left)
;                  (below bottom-right corner))))))

(paint (corner-split einstein 2))

;; Part c

;; The orientation of square-limit can be changed by passing different
;; transformations to square-of-four
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four identity flip-horiz
                                  flip-vert rotate180)))
    (combine4 (corner-split painter n))))

;; Original definition

;(define (square-limit painter n)
;  (let ((combine4 (square-of-four flip-horiz identity
;                                  rotate180 flip-vert)))
;    (combine4 (corner-split painter n))))

(paint (square-limit einstein 1))