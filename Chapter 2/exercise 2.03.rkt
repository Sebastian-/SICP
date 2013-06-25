#lang planet neil/sicp

;; Exercise 2.3

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

;; Rectangles ==========================

;; Rectangle represented as a length and width line segment
(define (make-rectangle length width)
  (cons length width))

;; Both get-length and get-width return line segments
(define (get-length rectangle)
  (car rectangle))

(define (get-width rectangle)
  (cdr rectangle))

;; Here a rectangle is represented by its diagonal
(define (make-rectangle diagonal)
  diagonal)

;; Again the selectors return the length and width line segments
(define (get-length rectangle)
  (let ((x1 (car (car rectangle)))
        (x2 (car (cdr rectangle)))
        (y1 (cdr (car rectangle)))
        (y2 (cdr (cdr rectangle))))
    (make-segment (make-point x1 y1) (make-point x2 y1))))

(define (get-width rectangle)
  (let ((x1 (car (car rectangle)))
        (x2 (car (cdr rectangle)))
        (y1 (cdr (car rectangle)))
        (y2 (cdr (cdr rectangle))))
    (make-segment (make-point x1 y1) (make-point x1 y2))))

;; Perimeter and Area procedures

(define (area rectangle)
  (let ((length (abs (- (car (car (get-length rectangle))) 
                        (car (cdr (get-length rectangle))))))
        (width (abs (- (cdr (car (get-width rectangle))) 
                       (cdr (cdr (get-width rectangle)))))))
    (* length width)))

(define (perimeter rectangle)
  (let ((length (abs (- (car (car (get-length rectangle))) 
                        (car (cdr (get-length rectangle))))))
        (width (abs (- (cdr (car (get-width rectangle))) 
                       (cdr (cdr (get-width rectangle)))))))
    (+ (* 2 length) (* 2 width))))