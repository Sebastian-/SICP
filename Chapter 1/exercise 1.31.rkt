#lang planet neil/sicp

;; Exercise 1.31

;; Part a ======================================================================

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

;; Defining factorial

(define (factorial n)
  (define (identity x) x)
  (define (inc x) (+ x 1))
  (product identity 1 inc n))

(factorial 4)
(factorial 5)
(factorial 6)
(factorial 7)

;; Approximating pi

(define (pi-product a b)
  (define (pi-term k)
    (if (even? k)
        (/ (+ 2.0 k)
           (+ 3.0 (- k 2)))
        (/ (+ 2.0 (- k 1))
           (+ 3.0 (- k 1)))))
  (define (inc x) (+ x 1))
  (* 4 (product pi-term a inc b)))

(pi-product 1 1000)
;; 3.143160705532257

;; Part b ======================================================================

(define (iter-product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (iter-factorial n)
  (define (identity x) x)
  (define (inc x) (+ x 1))
  (iter-product identity 1 inc n))

(iter-factorial 4)
(iter-factorial 5)
(iter-factorial 6)
(iter-factorial 7)

(define (iter-pi-product a b)
  (define (pi-term k)
    (if (even? k)
        (/ (+ 2.0 k)
           (+ 3.0 (- k 2)))
        (/ (+ 2.0 (- k 1))
           (+ 3.0 (- k 1)))))
  (define (inc x) (+ x 1))
  (* 4 (iter-product pi-term a inc b)))

(iter-pi-product 1 1000)

;; Oddly the pi approximation using the iterative definition of product outputs a slightly
;; different answer, as well as one extra digit