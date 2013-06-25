#lang planet neil/sicp

;; Exercise 1.29

(define (cube x)
  (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

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

;; Both evaluations return 1/4