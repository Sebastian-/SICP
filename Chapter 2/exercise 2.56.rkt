#lang racket

;; Exercise 2.56

;; Basic expression representation

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x) (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

;; Exponent representation

(define (make-exponentiation base ex)
  (cond [(=number? ex 0) 1]
        [(=number? ex 1) base]
        [(and (number? base) (number? ex)) (expt base ex)]
        [else (list '** base ex)]))

(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))

(define (base e) (cadr e))

(define (exponent e) (caddr e))

;; Derivative function
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ;; Exponentiation clause
        [(exponentiation? exp)
         (let [(u (base exp))
               (n (exponent exp))]
           (make-product
            (make-product n
                          (make-exponentiation u (- n 1)))
            (deriv u var)))]
        (else
         (error "unknown expression type: DERIV" exp))))

;; Testing
(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)

(deriv '(** 2 3) 'x)
(deriv '(** x 3) 'x)
(deriv '(** (+ x 3) 2) 'x)
(deriv '(** (+ x 3) 1) 'x)
(deriv '(** (* x y) 2) 'x)
