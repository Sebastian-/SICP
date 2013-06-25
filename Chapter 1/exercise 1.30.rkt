#lang planet neil/sicp

;; Exercise 1.30

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

;; Testing with exercise 1.29

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