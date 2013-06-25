#lang planet neil/sicp

;; Exercise 1.34

(define (f g)
  (g 2))

(f f)

;; Evaluating (f f) proceeds as follows:
;; (f f) -> (f 2) -> (2 2)
;; That is, argument g was assumed to be a procedure, but evaluation of (f f) yeilds the 
;; expression (f 2) -> (2 2). Two is not a procedure, so the evaluation fails.