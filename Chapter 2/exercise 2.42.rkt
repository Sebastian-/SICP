#lang planet neil/sicp

;; Exercise 2.42

;; To-do
;; Representation of sets of board positions
;;     adjoin-position (new-row, column, rest-of-queens)
;;     empty-board - representation of an empty board
;; safe? (k, positions) - determines whether a queen in kth column is safe with respect 
;; to the queens in the other positions

;; Positions
(define (new-position row column)
  (cons row column))

(define (get-row position)
  (car position))

(define (get-column position)
  (cdr position))

;; Set of Positions
(define empty-board (list ))

(define (adjoin-position row column rest-of-queens)
  (append rest-of-queens (new-position row column)))

;; Safe?
(define (safe? k positions)

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