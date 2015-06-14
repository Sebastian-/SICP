#lang racket

;; Exercise 2.85

(define (complex->real x)
  (make-real (real-part x)))
(put 'project '(complex) complex->real)

(define (real->rational x)
  (make-rational (round x) 1))
(put 'project '(real) real->rational)

(define (rational->integer x)
  (/ (numer x) (denom x)))
(put 'project '(rational) rational->complex)

(define (project x) (apply-generic 'project x))

(define (drop x)
  (if (get 'project (type-tag x))
      (let ([projection (project x)])
        (if (equ? (raise projection) x) 
            (drop projection)
            x))
      x))

;; To drop the result of apply generic, simply call it on (apply proc...)
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (drop (apply proc (map contents args)))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else (error "No method for these types"
                                     (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))



