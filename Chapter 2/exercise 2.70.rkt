#lang racket

;; Exercise 2.70

;; Running the code below returns an encoded length of 84. A fixed length
;; code for eight symbols would require 3 bits (2^3 = 8) per symbol. 
;; Therefore, encoding the rock song would take 3*(36 symbols) = 108 bits.

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) ; symbol
                               (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge tree)
  (if (null? (cdr tree)) 
      (car tree)
      (successive-merge (adjoin-set (make-code-tree (car tree) 
                                                    (cadr tree))
                                    (cddr tree)))))

(define (encode-symbol symbol tree)
  (cond [(leaf? tree) '()]
        [(contains? (symbols (left-branch tree)) symbol)
         (cons 0 (encode-symbol symbol (left-branch tree)))]
        [(contains? (symbols (right-branch tree)) symbol)
         (cons 1 (encode-symbol symbol (right-branch tree)))]
        [else (error "Symbol not found:" symbol)]))

(define (contains? list x)
  (cond [(null? list) false]
        [(equal? x (car list)) true]
        [else (contains? (cdr list) x)]))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define rock-tree
  (generate-huffman-tree '((A 2) (GET 2) (SHA 3) (WAH 1) 
                           (BOOM 1) (JOB 2) (NA 16) (YIP 9))))

(length (encode '(GET A JOB SHA NA NA NA NA NA NA NA NA
                  GET A JOB SHA NA NA NA NA NA NA NA NA
                  WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM) 
                rock-tree))