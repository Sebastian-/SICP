#lang planet neil/sicp

;; Exercise 2.32

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (l) (cons (car s) l)) rest)))))

;; Figured out the correct function to map by tracing out the desired
;; execution of the procedure with input (list 1 2 3). After the last 
;; recursive call to subsets is made, we hit the base case where s=null,
;; resulting in the first assignment to rest. At that point we have:
;;
;;     rest = ()     s = (3)
;;
;; and we want (append rest (map ?? rest)) to give (() (3)) (i.e. the
;; subsets of s = (3). So (map ?? rest) needs to return (3) somehow. 
;; Assuming this happens, lets move on to the next recursive step. Here,
;;
;;     rest = (() (3))     s = (2 3)
;;     
;; and we want (append rest (map ?? rest)) to give (() (3) (2) (2 3)), which
;; are again the subsets of s = (2 3). So, (map ?? rest) must return the list
;; ((2) (2 3)). This gives us a pretty big hint at what should be happening.
;; (map ?? rest) needs to generate the list of subsets with the newest element
;; that was just added to s in that recursive call. Hence, 
;;
;;     ?? = (lambda (l) (cons (car s) l))
;;     
;; And so, this procedure generates subsets by starting with the empty set,
;; then combining it with one element in the set. Then it takes the next
;; element and combines it with all the previously found subsets until all
;; elements have been used.

;; Test ================================================================

(equal? (subsets (list 1 2 3))
        (list '()
              (list 3)
              (list 2)
              (list 2 3)
              (list 1)
              (list 1 3)
              (list 1 2)
              (list 1 2 3)))