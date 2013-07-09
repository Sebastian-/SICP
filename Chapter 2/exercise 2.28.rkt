#lang planet neil/sicp

;; Exercise 2.28

;; An iterative solution I came up with first, followed by a recursive soltuion
;; which is much clearer (but does the same thing)

(define (fringe-iter list)
  (define (iter l leaves)
    (cond [(null? l) leaves]
          [(not (pair? l)) (cons l leaves)]
          [else (iter (cdr l) (append leaves (iter (car l) nil)))]))
  (iter list nil))

(define (fringe list)
  (cond [(null? list) nil]
        [(not (pair? list)) (cons list nil)]
        [else (append (fringe (car list)) (fringe (cdr list)))]))

;; Tests ===============================================================

(define x (list (list 1 2) (list 3 4)))

(fringe x)
(fringe (list x x))
        
