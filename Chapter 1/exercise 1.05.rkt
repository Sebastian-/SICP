#lang planet neil/sicp

;; Exercise 1.5

(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))

(test 0 (p))

;; Applicative-order
;;
;; Upon calling (test 0 (p)) the interpreter will try to evaluate the arguements to test
;; (namely (p)). But (p) evaluates to itself. The function (p) is not a primitive value, 
;; so the interpreter will carry on evaluating (p) indefinitely.

;; Normal-order
;;
;; In normal-order evaluation we do not carry out the evaluation of the arguments to 
;; functions, so when (test 0 (p)) is called, we get (if (= 0 0) 0 (p)), which evaluates 
;; to 0. We are never forced to evaluate (p), so the interpreter won't get stuck in an
;; infinte loop of evaluation.