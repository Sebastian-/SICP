#lang planet neil/sicp


;; Exercise 2.44

(define (split identity-placement split-placement)
  (define (split-procedure painter n)
    (if (= n 0)
        painter
        (let ((smaller (split-procedure painter (- n 1))))
          (identity-placement painter (split-placement smaller 
                                                       smaller)))))
  split-procedure)

(define up-split (split below beside))
(define right-split (split beside below))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(paint (up-split einstein 3))

(paint (right-split einstein 3))

(paint (corner-split einstein 3))