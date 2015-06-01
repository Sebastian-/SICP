#lang racket

;; Exercise 2.73

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;; Part a

;; We can't assimilate the predicates number? and variable? because they
;; don't dispatch to any procedures. In both cases constant values are 
;; returned.

;; Part b

;; Since we are dispatching on the algebraic operation, no type tagging is
;; required. In turn, the derivative functions must be altered slightly
;; since they are passed the operands instead of the whole expression. We
;; do this by changing the selectors for each addition and multiplication.

(define (install-deriv-package)
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
  (define (addend operands) (car operands))
  (define (augend operands) (cadr operands))
  (define (multiplicand operands) (car operands))
  (define (multiplier operands) (cadr operands))
  (define (sum-deriv operands)
    (make-sum (deriv (addend operands) var)
              (deriv (augend operands) var)))
  (define (product-deriv operands)
    (make-sum (make-product
               (multiplier operands)
               (deriv (multiplicand operands) var))
              (make-product
               (deriv (multiplier operands) var)
               (multiplicand operands)))))

(put 'deriv '(+) sum-deriv)
(put 'deriv '(*) product-deriv)

;; Part c

;; Installing exponentiation is similar to above and highlights how easily
;; extensible the system is.

(define (install-exp-deriv)
  (define (make-exponentiation base ex)
    (cond [(=number? ex 0) 1]
          [(=number? ex 1) base]
          [(and (number? base) (number? ex)) (expt base ex)]
          [else (list '** base ex)]))
  (define (base operands) (car operands))
  (define (exponent operands) (cadr operands))
  (define (exp-deriv operands)
    (let [(u (base exp))
          (n (exponent exp))]
      (make-product
       (make-product n
                     (make-exponentiation u (- n 1)))
       (deriv u var)))))

(put 'deriv '(**) exp-deriv)

;; Part d

;; Simply reverse the order of keys in the put statements

(put '(+) 'deriv sum-deriv)
(put '(*) 'deriv product-deriv)
(put '(**) 'deriv exp-deriv)


