#lang planet neil/sicp

;; Exercise 2.27

(define (reverse l)
  (define (iter list rev)
    (if (null? list)
        rev
        (iter (cdr list) (cons (car list) rev))))
  (iter l nil))

(define (deep-reverse list)
  (define (iter l rev)
    (cond [(null? l) rev]
          [(pair? (car l)) (iter (cdr l) (cons (deep-reverse (car l)) rev))]
          [else (iter (cdr l) (cons (car l) rev))]))
  (iter list nil))


;; Tests ===============================================================

(define x (list (list 1 2) (list 3 4)))

x
(reverse x)
(deep-reverse x)
