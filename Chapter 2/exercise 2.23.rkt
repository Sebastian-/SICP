#lang planet neil/sicp

;; Exercise 2.23

(define (for-each proc list)
  (cond [(null? list) true]
        [else (proc (car list))
              (for-each proc (cdr list))]))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))