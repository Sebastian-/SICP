#lang planet neil/sicp

;; Exercise 2.51

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                  new-origin
                  (vector-sub (m corner1) new-origin)
                  (vector-sub (m corner2) new-origin)))))))

(define (below bottomP topP)
  (let [(split-point (make-vect 0 0.5))]
    (let [(paint-top
           (transform-painter topP
                              split-point
                              (make-vect 1 0.5)
                              (make-vect 0 1)))
          (paint-bot
           (transform-painter bottomP
                              (make-vect 0 0)
                              (make-vect 1 0)
                              split-point))]
    (lambda (frame)
      (paint-top frame)
      (paint-bot frame)))))

(define (below-rotation bottomP topP)
  (rotate270 (beside (rotate90 topP) (rotate90 bottomP))))
   

(paint einstein)
(paint diagonal-shading)
(paint (below einstein diagonal-shading))
(paint (below-rotation einstein diagonal-shading))