#lang planet neil/sicp

;; Exercise 2.14

;; Data abstraction ====================================================

(define (make-interval a b) (cons a b))

(define (upper-bound interval)
  (cdr interval))

(define (lower-bound interval)
  (car interval))

(define (make-center-percent center percent)
  (let ((lower-bound (* center (- 1 (/ percent 100.0))))
        (upper-bound (* center (+ 1 (/ percent 100.0)))))
    (make-interval lower-bound upper-bound)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (percent i)
  (* (- (/ (upper-bound i) (center i)) 1) 100))

;; Procedures ==========================================================

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;; Lem E. Tweakit's formulas ===========================================

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))
(define r1 (make-interval 99 101))
(define r2 (make-interval 199 201))

(define ans1 (par1 r1 r2))
(define ans2 (par2 r1 r2))

(center ans1)
(percent ans1)
(center ans2)
(percent ans2)

;; Results =============================================================
;;
;; 66.67963020578692
;; 2.1663750437434404
;; 66.66629627983465
;; 0.8333416670833538
;;
;; The answers, as well as their errors, are indeed different. This stems from the fact that
;; par1 uses the inexact resistance values more often than par2 in its calculations. This 
;; results in a larger error, as each arithmetic operation introduces some error. 

