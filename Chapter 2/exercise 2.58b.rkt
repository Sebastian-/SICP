#lang racket

;; Exercise 2.58 - Part b

;; My first attempt was misguided in trying to change make-sum/product.
;; The core problem is going from infix notation (1 + 2 * 5) to prefix
;; notation (make-sum 1 (make-product 2 5)). Changing make-sum/product
;; does nothing to solve that. Instead, the predicates must change to 
;; recognize when to call make-sum/prodct when parsing an infix exp. Once 
;; the problem is viewed this way it becomes much easier. To ensure 
;; multiplication is done before addition, generate all of the make-sums 
;; before any make-products (as above). This is reflected in the code for
;; the sum? and prodcut? predicates.

(define (contains? exp op)
  (cond [(null? exp) false]
        [(equal? (car exp) op) true]
        [else (contains? (cdr exp) op)]))

(define (before exp op)
  (cond [(null? exp) exp]
        [(equal? (car exp) op) null]
        [else (cons (car exp) (before (cdr exp) op))]))

(define (after exp op)
  (cond [(null? exp) null]
        [(equal? (car exp) op) (cdr exp)]
        [else (after (cdr exp) op)]))

;; If there is only one value in a list, simply return that value 
(define (extract l)
  (if (null? (cdr l))
      (car l)
      l))

(define (sum? x) 
  (and (pair? x) (contains? x '+)))

(define (addend s) (extract (before s '+)))

(define (augend s) (extract (after s '+)))

(define (product? x)
  (cond [(sum? x) false]
        [else (and (pair? x) (contains? x '*))]))

(define (multiplier p) (extract (before p '*)))

(define (multiplicand p) (extract (after p '*)))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

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

(deriv '(x + 3 * (x + y + 2)) 'x)
(deriv '(3 * x * 5 + 2) 'x)
(deriv '(2 * x * x + (x + x + 5) + (5 * x) * 2) 'x)