#lang planet neil/sicp

;; Exercise 2.20

(define (same-parity i . l)
  (define (iter first list same-par)
    (if (null? list)
        (cons first same-par)
        (if (= (remainder first 2) (remainder (car list) 2))
            (iter first (cdr list) (cons (car list) same-par))
            (iter first (cdr list) same-par))))
  (iter i (reverse l) nil))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)