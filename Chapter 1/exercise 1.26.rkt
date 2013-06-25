#lang planet neil/sicp

;; Exercise 1.26

;; Original Implementation =====================================================
(define (square x)
  (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

;; Louis Reasoner Implementation ===============================================

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base
                                    (- exp 1)
                                    m))
                    m))))

;; The reason for the change in efficiency of these two processes hinges on the way 
;; expressions are evaluated. Namely, arguments are evaluated before function application
;; occurs. So, for every recursive call in the first implementation 
;; (expmod base (/ exp 2) m) only needs to be computed once, whereas in Louis's 
;; implementation it must be evaluated twice. 