#lang planet neil/sicp

;; Exercise 2.43

;(flatmap
; (lambda (new-row)
;   (map (lambda (rest-of-queens)
;          (adjoin-position new-row 
;                           k 
;                           rest-of-queens))
;        (queen-cols (- k 1))))
; (enumerate-interval 1 board-size))

;; versus 

;(flatmap
; (lambda (rest-of-queens)
;   (map (lambda (new-row)
;          (adjoin-position new-row
;                           k
;                           rest-of-queens))
;        (enumerate-interval 1 board-size)))
; (queen-cols (- k 1)))

;; these two loops both produce a set of board positions the rest of the
;; function must check. However, the first one is much slower because it 
;; unnecessarily calls queen-cols for every row in the board. This means
;; that a call to (queen-cols k) will generate (board-size - 1) calls to 
;; (queen-cols (- k 1)) which in turn will generate (board-size - 2) calls 
;; and so on. This is a tree recursive process, which means that we will 
;; have to go from (queen-cols k) to (queen-cols 0) (board-size - 1)! times, 
;; compared to the one time in the original loop. This suggests that the 
;; new running time will be something like (board-size - 1)! * T. Code below 
;; can be used to compare times empirically by calling either (timed queens n) 
;; or (timed slow-queens n). As of 2015 on a laptop, calling slow-queens for
;; n > 9 takes a while...

(define (timed procedure n)
  (start procedure n (runtime)))

(define (start procedure n start-time)
  (procedure n)
  (report n (- (runtime) start-time)))

(define (report n elapsed-time)
  (display n)
  (display " Queens *** ")
  (display elapsed-time))

;; Position representation
(define (new-position row column)
  (cons row column))

(define (get-row position)
  (car position))

(define (get-column position)
  (cdr position))

(define empty-board nil)

(define (adjoin-position row column rest-of-queens)
  (cons (new-position row column) rest-of-queens))

;; Safe?
(define (safe? k positions)
  (not (or (row-conflict? k positions)
           (diagonal-conflict? k positions))))

;; Returns true if the queen positions share the same row as the kth queen
;; false otherwise
(define (row-conflict? k positions)
  (let ([k-queen-row (get-row (car (filter (lambda (position)
                                             (= (get-column position) k))
                                           positions)))]
        [positions-to-check (filter (lambda (position)
                                      (not (= (get-column position) k)))
                                    positions)])
    (not (null? (filter (lambda (position)
                          (= (get-row position) k-queen-row))
                        positions-to-check)))))

;; Returns true if a queen lies on the diagonal of the kth queen, false
;; otherwise
(define (diagonal-conflict? k positions)
  (let ([k-queen-row (get-row (car (filter (lambda (position)
                                             (= (get-column position) k))
                                           positions)))]
        [positions-to-check (filter (lambda (position)
                                      (not (= (get-column position) k)))
                                    positions)])
    (not (null? (filter (lambda (position)
                          (= (abs (- k (get-column position)))
                             (abs (- k-queen-row (get-row position)))))
                        positions-to-check)))))


;; Main function
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row
                                    k
                                    rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (slow-queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (adjoin-position new-row 
                                    k 
                                    rest-of-queens))
                 (queen-cols (- k 1))))
          (enumerate-interval 1 board-size)))))
  (queen-cols board-size))


;; Abstract list functions
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))