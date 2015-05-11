#lang planet neil/sicp

;; Exercise 2.54

(define (equal? list1 list2)
  (cond [(and (null? list1) (null? list2)) true]
        [(or (null? list1) (null? list2)) false]
        [else (if (eq? (car list1) (car list2))
                  (equal? (cdr list1) (cdr list2))
                  false)]))

;; Testing

(equal? '(a b c d) '(a b c d))
(equal? '(a b c) '(a b c d))
(equal? '(a b c d) '(a b c))
(equal? '() '())
(equal? '(a b c e) '(a b c d))
