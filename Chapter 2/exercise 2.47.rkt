#lang planet neil/sicp

;; Exercise 2.47

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (frame-origin frame)
  (car frame))

(define (frame-edge1 frame)
  (car (cdr frame)))

(define (frame-edge2 frame)
  (car (cdr (cdr frame))))
  

(define (make-altframe origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (altframe-origin frame)
  (car frame))

(define (altframe-edge1 frame)
  (car (cdr frame)))

(define (altframe-edge2 frame)
  (cdr (cdr frame)))

(define f1 (make-frame 1 2 3))
(define f2 (make-altframe 1 2 3))

(frame-origin f1)
(frame-edge1 f1)
(frame-edge2 f1)

(altframe-origin f2)
(altframe-edge1 f2)
(altframe-edge2 f2)