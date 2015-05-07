#lang planet neil/sicp

;; Exercise 2.50

;; The transform-painter in the book is preferrable to the one in the 
;; picture language package because it maps from painter to painter, 
;; rather than painter to procedure which takes a painter.
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                  new-origin
                  (vector-sub (m corner1) new-origin)
                  (vector-sub (m corner2) new-origin)))))))


(define (flip-horiz painter)
  (transform-painter painter 
                     (make-vect 1 0)
                     (make-vect 0 0)
                     (make-vect 1 1)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1 1)
                     (make-vect 0 1)
                     (make-vect 1 0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0 1)
                     (make-vect 0 0)
                     (make-vect 1 1)))

(paint einstein)
(paint (flip-horiz einstein))
(paint (rotate180 einstein))
(paint (rotate270 einstein))