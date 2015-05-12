#lang planet neil/sicp

;; Exercise 2.55

;; The expression:
(car ''abracadabra)

;; Is equivalent to:
(car '(quote (abracadabra)))

;; so the interpreter sees '(quote (abracadabra)) as a list of symbols, 
;; the car of which is the symbol quote. Therefore 'quote is printed out.
