#lang racket

;; Exercise 2.59

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (union-set s1 s2)
  (cond [(null? s1) s2]
        [(null? s2) s1]
        [(not (element-of-set? (car s1) s2))
         (cons (car s1) (union-set (cdr s1) s2))]
        [else (union-set (cdr s1) s2)]))

;; Testing

(union-set '() '())
(union-set '(1 2 3) '())
(union-set '() '(1 2 3))
(union-set '(1 2) '(3 4))
(union-set '(1 2) '(2 3 4))
(union-set '(1 2) '(1 2))