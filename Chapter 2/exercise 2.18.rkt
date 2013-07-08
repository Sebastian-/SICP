#lang planet neil/sicp

;; Exercise 2.18

(define (reverse l)
  (define (iter list rev)
    (if (null? list)
        rev
        (iter (cdr list) (cons (car list) rev))))
  (iter l nil))

(reverse (list 1 4 9 16 25))