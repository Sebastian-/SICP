#lang planet neil/sicp

;; Exercise 2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;; Part a ==============================================================

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))
  
(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

;; Part b ==============================================================

(define (branch-weight branch)
  (if (pair? (branch-structure branch))
      (total-weight (branch-structure branch))
      (branch-structure branch)))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define branch-5 (make-branch 1 5))
(define mobile-10 (make-mobile branch-5 branch-5))
(define branch-10 (make-branch 1 mobile-10))
(define mobile-15l (make-mobile branch-10 branch-5))
(define mobile-15r (make-mobile branch-5 branch-10))
(define mobile-20 (make-mobile branch-10 branch-10))
(define branch-20 (make-branch 1 mobile-20))
(define mobile-40 (make-mobile branch-20 branch-20))

(total-weight mobile-40)
(total-weight mobile-20)
(total-weight mobile-15l)
(total-weight mobile-15r)
(total-weight mobile-10)

;; Part c ==============================================================

(define (branch-balanced? branch)
  (if (pair? (branch-structure branch))
      (balanced? (branch-structure branch))
      true))

(define (balanced? mobile)
  (and (= (* (branch-length (left-branch mobile))
             (branch-weight (left-branch mobile)))
          (* (branch-length (right-branch mobile))
             (branch-weight (right-branch mobile))))
       (branch-balanced? (left-branch mobile))
       (branch-balanced? (right-branch mobile))))

(balanced? mobile-40)
(balanced? mobile-20)
(balanced? mobile-15l)
(balanced? mobile-15r)
(balanced? mobile-10)

;; Part d ==============================================================

;; The new representation:

(define (new-make-mobile left right)
  (cons left right))

(define (new-make-branch length structure)
  (cons length structure))

;; The only part of our programs that deals directly with the implementation
;; of the mobiles/branches are the selectors. So, only their implementation
;; would change:

(define (new-right-branch mobile)
  (cdr mobile))

(define (new-branch-structure branch)
  (cdr branch))