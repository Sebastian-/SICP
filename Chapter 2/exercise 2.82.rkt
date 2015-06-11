#lang racket

;; Exercise 2.82

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (let ((coerced-args (coerce args type-tags)))
            (if coerced-args
                (apply-generic op coerced-args)
                (error "No method for these types")))))))

;; Returns true if arg can be coerced to target-type, fasle otherwise
(define (can-coerce? arg target-type)
  (or (equals (type-tag arg) target-type)
      (get-coercion (type-tag arg) target-type)))

;; Returns true if all arguments could be coerced to target-type, 
;; false otherwise.
(define (coerce-list? args target-type)
  (cond [(null? args) true]
        [(can-coerce? (car args) target-type) 
         (coerce-list? (cdr args) target-type)]
        [else false]))

;; Returns a list of uniformly coerced arguments, or false if failed
(define (coerce args type-tags)
  (cond [(null? type-tags) false]
        [(coerce-list? args (car type-tags))
         (map (lambda (x) (if (equal? (type-tag x) (car type-tags))
                              x ;; arg is already the target type
                              ((get-coercion (type-tag x) 
                                             target-type)
                               ;; get the coercion procedure
                               x))) ;; appy to the argument
              args)]
        [else (coerce args (cdr type-tags))]))



;; Since this strategy will only coerce to one type at a time, many mixed
;; type operations will be missed. In fact, a mixed type operation will 
;; only be used if the arguments are already arranged to make use of it.
;; The coercion search strategy will not find one otherwise.

;; For example, if the arguments are types 'complex 'rational, and there
;; exists an operation which would work if the types were reversed, this 
;; coercion strategy will not find it. Instead it will coerce the 
;; arguments to be either 'complex 'complex or 'rational 'rational.