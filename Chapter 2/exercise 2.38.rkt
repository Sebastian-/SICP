#lang planet neil/sicp

;; Exercise 2.38

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(fold-right / 1 (list 1 2 3))
;; Result: 3/2 or (3/2)/1

(fold-left / 1 (list 1 2 3))
;; Result: 1/6 or (1/2)/3

(fold-right list nil (list 1 2 3))
;; Result: (1 (2 (3 ())))

(fold-left list nil (list 1 2 3))
;; Result: (((() 1) 2) 3)

;; The only difference between fold-left and fold-right is the order in which 
;; they operate on a sequence. So, to guarantee that they produce the same value, 
;; op must be associative. For example, addition and multiplication are associative:

(equal? (fold-left * 1 (list 1 2 3))
        (fold-right * 1 (list 1 2 3)))

(equal? (fold-left + 1 (list 1 2 3))
        (fold-right + 1 (list 1 2 3)))