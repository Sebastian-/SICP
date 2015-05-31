#lang racket

;; Exercise 2.72

;; For the special case mentioned in exercise 2.71, the complexity of 
;; encode-symbol is fairly straightforward and is entirely driven by the 
;; size of the symbol alphabet. At every level in the tree, the algorithm 
;; calls 'contains?' (which is O(n)) over the rest of the symbols left in 
;; the subtrees. Therefore, in the worst case, contains? searches over 
;; n + n-1 + n-2 +... 2 + 1 symbols which results in O(n^2) complexity. In
;; the best case, when searching for the most frequent symbol, encode is
;; essentially O(1), as finding the symbol in the right subtree is 
;; independent of how many other symbols there may be.

(define (encode-symbol symbol tree)
  (cond [(leaf? tree) '()]
        [(contains? (symbols (right-branch tree)) symbol)
         (cons 1 (encode-symbol symbol (right-branch tree)))]
        [(contains? (symbols (left-branch tree)) symbol)
         (cons 0 (encode-symbol symbol (left-branch tree)))]
        [else (error "Symbol not found:" symbol)]))

(define (contains? list x)
  (cond [(null? list) false]
        [(equal? x (car list)) true]
        [else (contains? (cdr list) x)]))