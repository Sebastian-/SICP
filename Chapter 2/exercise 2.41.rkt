#lang planet neil/sicp

;; Exercise 2.41

;; Helpers =============================================================

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

;; Triple Sums Equal to s ==============================================

(define (distinct-triples n)
  (flatmap (lambda (i) 
             (flatmap (lambda (j) 
                        (map (lambda (k) (list i j k)) 
                             (enumerate-interval 1 (- j 1)))) 
                      (enumerate-interval 2 (- i 1))))
           (enumerate-interval 3 n)))

(define (sum-equal? s triple)
  (= (+ (car triple) (cadr triple) (caddr triple)) s))

(define (triple-sums-equal-to s n)
  (filter (lambda (triple) (sum-equal? s triple))
          (distinct-triples n)))

(triple-sums-equal-to 10 5)
              
              