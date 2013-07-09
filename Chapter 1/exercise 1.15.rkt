#lang planet neil/sicp

;; Exercise 1.15

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;; Part a ==============================================================

;; (sine 12.15)
;; (p (sine 4.05))
;; (p (p (sine 1.35)))
;; (p (p (p (sine 0.45))))
;; (p (p (p (p (sine 0.15)))))
;; (p (p (p (p (p (sine 0.05))))))

;; So, p is applied five times.

;; Part b ==============================================================

;; The order of growth in both time and space is related to the number of calls made to p.
;; From inspection we can see that this is related to the number of times we can divide the
;; input angle by 3. So, the order of growth for both time and space is O(log angle)