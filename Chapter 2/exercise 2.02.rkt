#lang planet neil/sicp

;; Exercise 2.2

;; Points ==============================

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

;; Line Segments =======================

(define (make-segment start end)
  (cons start end))

(define (start-segment line)
  (car line))

(define (end-segment line)
  (cdr line))

;; =====================================

(define (midpoint-segment line)
  (define (average x y)
    (/ (+ x y) 2))
  (let ((x1 (car (car line)))
        (x2 (car (cdr line)))
        (y1 (cdr (car line)))
        (y2 (cdr (cdr line))))
  (make-point (average x1 x2) (average y1 y2))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define line (make-segment (make-point 0 0) (make-point 3 4)))
(print-point (start-segment line))
(print-point (end-segment line))
(print-point (midpoint-segment line))