#lang racket

;; Exercise 2.84

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((a1 (car args))
                    (a2 (cadr args)))
                (let ((a1-raised (can-raise? a1 a2))
                      (a2-raised (can-raise? a2 a1)))
                  (cond [(a1-raised) (apply-generic op a1-raised a2)]
                        [(a2-raised) (apply-generic op a1 a2-raised)]
                        [else (error "No method for these types")])))                
              (error "No method for these types"
                     (list op type-tags)))))))

;; Returns arg1 raised to the type of arg2, if possible. False otherwise
(define (can-raise? arg1 arg2)
  (cond [(equal? (type-tag arg1) (type-tag arg2)) arg1]
        [(get 'raise (type-tag arg1)) (can-raise? (raise arg1) arg2)]
        [else false]))

