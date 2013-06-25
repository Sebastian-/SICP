#lang planet neil/sicp

;; Exercise 2.6

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; First lets figure out how zero and adding 1 work
(add-1 zero)
(add-1 (lambda (f) (lambda (x) x)))
                             ;;-----zero------------|
(lambda (f) (lambda (x) (f ((lambda (f) (lambda (x) x)) f) x)))
(lambda (f) (lambda (x) (f ((lambda (x) x) x))))
(lambda (f) (lambda (x) (f x)))

;; It apppears that adding 1 is equivalent to applying f to some x once. So we have

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

;; To add two numbers a and b we would want to generate a procedure with one argument
;; which applies this argument to some x, a+b times

(define (+ a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

;; The key part of this procedure is ((a f) ((b f) x)). The subexpression ((b f) x) will
;; apply the function f to x b times. Then (a f) will further apply f another a times.