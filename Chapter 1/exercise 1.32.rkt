#lang planet neil/sicp

;; Exercise 1.32

;; Part a ======================================================================

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

;; Sum

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (cube x)
  (* x x x))

(define (simpsons-integral f a b n)
  (define h (/ (- b a) n))
  (define (odd-index? i)
    (odd? (/ (- i a) h)))
  (define (simpson-term k)
    (cond [(or (= k a) (= k b)) (f k)]
          [(odd-index? k) (* 4 (f k))]
          [else (* 2 (f k))]))
  (define (next k)
    (+ k h))
  (* (/ h 3) (sum simpson-term a next b)))

(simpsons-integral cube 0 1 100)
(simpsons-integral cube 0 1 1000)

;; Product

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (factorial n)
  (define (identity x) x)
  (define (inc x) (+ x 1))
  (product identity 1 inc n))

(factorial 4)
(factorial 5)
(factorial 6)
(factorial 7)

;; Part b ======================================================================

(define (iter-accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

;; Sum

(define (iter-sum term a next b)
  (iter-accumulate + 0 term a next b))

(define (iter-simpsons-integral f a b n)
  (define h (/ (- b a) n))
  (define (odd-index? i)
    (odd? (/ (- i a) h)))
  (define (simpson-term k)
    (cond [(or (= k a) (= k b)) (f k)]
          [(odd-index? k) (* 4 (f k))]
          [else (* 2 (f k))]))
  (define (next k)
    (+ k h))
  (* (/ h 3) (iter-sum simpson-term a next b)))

(iter-simpsons-integral cube 0 1 100)
(iter-simpsons-integral cube 0 1 1000)

;; Product

(define (iter-product term a next b)
  (iter-accumulate * 1 term a next b))

(define (iter-factorial n)
  (define (identity x) x)
  (define (inc x) (+ x 1))
  (product identity 1 inc n))

(iter-factorial 4)
(iter-factorial 5)
(iter-factorial 6)
(iter-factorial 7)