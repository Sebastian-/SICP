#lang planet neil/sicp

;; Exercise 1.20

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; applicative order - 4 remainder operations

(gcd 206 40)
(gcd 40 (remainder  206 40))
(gcd 40 6)
(gcd 6 (remainder 40 6))
(gcd 6 4)
(gcd 4 (remainder 6 4))
(gcd 4 2)
(gcd 2 (remainder 4 2))
(gcd 2 0)
2

;; normal order - 18 remainder operations

(gcd 206 40)
(gcd 40 (remainder 206 40))
;; 1 remainder operation in the if
(gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
;; 2 remainder operations in the if predicate
(gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) 
                                                  (remainder 40 (remainder 206 40))))
;; 4 remainder operations in the if predicate
(gcd (remainder (remainder 206 40) 
                (remainder 40 (remainder 206 40)))
     (remainder (remainder 40 (remainder 206 40)) 
                (remainder (remainder 206 40) 
                           (remainder 40 (remainder 206 40)))))
;; 7 remainder operations in the if predicate
(remainder (remainder 206 40) 
           (remainder 40 (remainder 206 40)))
2