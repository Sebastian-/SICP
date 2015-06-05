#lang racket

;; Exercise 2.77

;; This question seems a little misleading, and makes me wonder if I've
;; missed something. It states that we need to include all the different 
;; selectors for complex numbers, when 
;; (put 'magnitude '(complex) magnitude) seems to be enough for this case.
;; I suppose they may simply have included them to suggest that all 
;; selectors require similar treatment.

;; The definition of apply generic given in the book
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these types: APPLY-GENERIC"
                (list op type-tags))))))

;; Execution when (magnitude z) is called
(magnitude z)
(magnitude ('complex ('rectangular (3 4))))

;; Fetching the generic magnitude function, this is the function added by
;; (put 'magnitude '(complex) magnitude)
(apply-generic 'magnitude ('complex ('rectangular (3 4))))
(let ([type-tags '(complex)])
  (let ([proc (get 'magnitude '(complex))])
    (apply magnitude ('rectangular (3 4)))))

;; Fetching the rectanular magnitude function that was already in the 
;; rectangular package.
(apply-generic 'magnitude (rectangular (3 4)))
(let ([type-tags '(rectangular)])
  (let ([proc (get 'magnitude '(rectangular))])
    (apply magnitude (3 4))))

;; From here the basic magnitude function is applied and should return 5
;; Two calls to apply-generic were used, one for each type tag.