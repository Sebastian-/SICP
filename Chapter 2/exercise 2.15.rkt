#lang racket

;; Exercise 2.15

;; Data abstraction ====================================================

(define (make-interval a b) (cons a b))

(define (upper-bound interval)
  (cdr interval))

(define (lower-bound interval)
  (car interval))

;; Procedures ==========================================================

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

;; Example

(define one-hundred (make-interval 100 100))
(define two-hundred (make-interval 200 200))
(define one-hundred-err (make-interval 99 101))
(define two-hundred-err (make-interval 199 201))
(define three-hundred (add-interval one-hundred two-hundred))
(define three-hundred-err (add-interval one-hundred-err two-hundred-err))

(upper-bound three-hundred)
(lower-bound three-hundred)
(upper-bound three-hundred-err)
(lower-bound three-hundred-err)

;; Results =============================================================

;; 300
;; 300
;; 302
;; 298

;; Yes, Eva Lu Ator is right. Every arithmetic operation with uncertain values introduces
;; some additional error as the width of the interval will either stay the same or
;; increase

