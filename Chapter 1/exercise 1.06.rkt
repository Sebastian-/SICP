#lang planet neil/sicp

;; Exercise 1.6

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

;; The reason if is a special form is so that its "arguments" (i.e. the then-clause and
;; else-clause) are not evaluated until the predicate has been evaluated. Since this 
;; new-if procedure is evaluated like any other procedure, the interpreter will try to 
;; evaluate the then-clause and else-clause and reduce them to a primitive value before
;; entering the body of new-if. This works fine when the arguments can be reduced to 
;; primitives, but in the case above, the interpreter will be stuck in an infinte loop
;; of evaluation as it tries to evaluate the else-clause "sqrt-iter"