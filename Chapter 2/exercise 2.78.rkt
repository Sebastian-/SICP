#lang racket

;; Exercise 2.78

;; We no long wish to tag ordinary numbers with anything, so simply return
;; the number
(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

;; The generic implementation still makes use of the 'scheme-number tag,
;; so it must still be returned. We have only changed the representation
;; of numbers, not how it interacts with the system above it.
(define (type-tag datum)
  (cond [(number? datum) 'scheme-number]
        [(pair? datum) (car datum)]
        [else (error "Bad tagged datum: TYPE-TAG" datum)]))

;; Since there is no more type tag to strip away for a number, it is
;; simply returned.
(define (contents datum)
  (cond [(number? datum) datum]
        [(pair? datum) (cdr datum)]
        [else (error "Bad tagged datum: CONTENTS" datum)]))
