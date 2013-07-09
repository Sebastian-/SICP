#lang planet neil/sicp

;; Exercise 2.22

(define (square x)
  (* x x))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

(square-list (list 1 2 3 4))
 
;; Results in: (mcons 16 (mcons 9 (mcons 4 (mcons 1 '()))))

;; The answer list is created from end to front, whereas the things list is processed
;; from front to end. So, the first element of the things list will be the last element in
;; the answer list, and so on.

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

(square-list (list 1 2 3 4))
 
;; Results in: (mcons (mcons (mcons (mcons '() 1) 4) 9) 16)

;; Reversing the arguments to cons does not solve the reversal problem, it doesn't even
;; generate a list of the proper form. Here, the answer list will just be a pair, with 
;; the first element being another list, and the second being the last element that was
;; processed.
