#lang racket

;; Exercise 2.58 - Part b (first attempt) INCORRECT!

;; The basic idea is this:

;; (make-sum/product exp1 exp2) = (append (representation for exp1)
;;                                        '(+ or *)
;;                                        (representation for exp2))

;; Each exp is reduced until it is either a number or variable. Order of
;; operations is maintained in make-product by making each sum 
;; subexpression its own list. (NEVERMIND, CURRENTLY BUGGED. Expressions
;; such as 3 * x * 5 + 2 are incorrectly parsed as 3 * x * 7)

;; Predicates are unchanged from part a

;; Selectors follow simply from the representation above. exp1 is simply
;; the car of the sum/product, and exp2 is everything that comes after the
;; + or *
 
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
        [else (let [(left (cond [(or (number? a1) (variable? a1)) 
                                 (list a1)]
                                [(sum? a1)
                                 (make-sum (addend a1) (augend a1))]
                                [(product? a1)
                                 (make-product (multiplier a1) 
                                               (multiplicand a1))]))
                    (right (cond [(or (number? a2) (variable? a2)) 
                                  (list a2)]
                                 [(sum? a2)
                                  (make-sum (addend a2) (augend a2))]
                                 [(product? a2)
                                  (make-product (multiplier a2) 
                                                (multiplicand a2))]))]
                (append left '(+) right))]))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        [else (let [(left (cond [(or (number? m1) (variable? m1)) 
                                 (list m1)]
                                [(sum? m1)
                                 (list (make-sum (addend m1) (augend m1)))]
                                [(product? m1)
                                 (make-product (multiplier m1) 
                                               (multiplicand m1))]))
                    (right (cond [(or (number? m2) (variable? m2)) 
                                  (list m2)]
                                 [(sum? m2)
                                  (list (make-sum (addend m2) (augend m2)))]
                                 [(product? m2)
                                  (make-product (multiplier m2) 
                                                (multiplicand m2))]))]
                (append left '(*) right))]))

(define (sum? x) (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cddr s)))

(define (product? x) (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) 
  (if (null? (cdddr p))
      (caddr p)
      (cddr p)))

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
        (else
         (error "unknown expression type: DERIV" exp))))

;; Testing

;(make-product 3 (make-sum (make-product 'x 5) 2))
;(deriv '(3 * x * 5 + 2) 'x)

(make-sum 'a (make-sum 'b (make-sum 'c 'd)))
(make-sum (make-sum (make-sum 'a 'b) 'c) 'd)
(make-sum (make-sum 'a 'b) (make-sum 'c 'd))
(make-sum 'x (make-sum 'y 2))

(make-sum 'x (make-product 3 (make-sum 'x (make-sum 'y 2))))
;
;(addend '(x + 3 * (x + y + 2)))
;(augend '(x + 3 * (x + y + 2)))
;
;(multiplier '(3 * (x + y + 2)))
;(multiplicand '(3 * (x + y + 2)))

;(deriv '(x + 3 * (x + y + 2) * 5 + 2) 'x)
;(deriv '(3 + 5 * 4 + 3 * x) 'x)
;(deriv '(x * 3 + 5 * (x + y + 2)) 'x)