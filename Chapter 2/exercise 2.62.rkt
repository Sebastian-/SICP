#lang racket

;; Exercise 2.62

(define (union-set s1 s2)
  (cond [(null? s1) s2]
        [(null? s2) s1]
        [else (let [(x1 (car s1))
                    (x2 (car s2))]
                (cond [(= x1 x2) (cons x1 (union-set (cdr s1) (cdr s2)))]
                      [(> x1 x2) (cons x2 (union-set s1 (cdr s2)))]
                      [(< x1 x2) (cons x1 (union-set (cdr s1) s2))]))]))

;; Testing 

(union-set '() '())
(union-set '(1 2 3) '())
(union-set '() '(1 2 3))
(union-set '(1 2) '(3 4))
(union-set '(1 2) '(2 3 4))
(union-set '(1 2) '(1 2))
(union-set '(5 6) '(1 2))
(union-set '(5 6 7) '(1 2 9 11))