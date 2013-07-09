#lang planet neil/sicp

;; Exercise 1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10)
;; 1024

(A 2 4)
;; 65536

(A 3 3)
;; 65536

(define (f n) (A 0 n))

;; f(n) = 2n


(define (g n) (A 1 n))

; (A 1 n)
; (A 0 (A 1 (- n 1)))
; (* 2 (A 1 (- n 1)))
; ...
; (* 2 (* 2 (* 2 ... (A 1 1))))
; (* 2 (* 2 (* 2 ... 2)))

;; g(n) = 2^n

(define (h n) (A 2 n))
; (A 2 n)
; (A 1 (A 2 (- n 1)))
;; From the previous question we have that (A 1 n) = 2^n, so this suggests that
;; h(n) = 2^h(- n 1)

; (A 1 (A 1 (A 2 (- n 2))))
; ...
; (A 1 (A 1 (A 1 ... (A 2 1))))
; (A 1 (A 1 ... (A 1 2)))
; (A 1 (A 1 ... (A 0 (A 1 1))))
; (A 1 (A 1 ... (A 0 2)))
; (A 1 (A 1 ... (* 2 2)))
; ...

;; h(n) = 2^(2^(2^(...(2^2))) (n twos)
;; or h(n) = 2^(h(- n 1)), h(1) = 2