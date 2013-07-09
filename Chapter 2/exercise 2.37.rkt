#lang planet neil/sicp

;; Exercise 2.37

;; Accumulator Definitions =============================================

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;; Matrix Operations ===================================================

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product v row))  m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

;; Tests ===============================================================

(define matrix (list (list 1 2 3 4)
                     (list 4 5 6 6)
                     (list 6 7 8 9)))

(define vector (list 1 2 3 4))

(equal? (dot-product vector vector)
        30)
(equal? (matrix-*-vector matrix vector)
        (list 30 56 80))
(equal? (transpose matrix)
        (list (list 1 4 6)
              (list 2 5 7)
              (list 3 6 8)
              (list 4 6 9)))
(equal? (matrix-*-matrix matrix (transpose matrix))
        (list (list 30 56 80)
              (list 56 113 161)
              (list 80 161 230)))