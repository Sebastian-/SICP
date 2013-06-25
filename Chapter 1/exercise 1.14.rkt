#lang planet neil/sicp

;; Exercise 1.14

(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;; As the book mentions in the previous section, the space complexity of a process is 
;; generally related to the depth of its process tree, while the time complexity is 
;; related to the number of nodes in the tree. Upon drawing out the tree we can see that
;; as the process evolves, the deepest branch of the tree will decrement the number of 
;; coins until it reaches 1, then decrement the amount until it reaches 0. This could also
;; have been deduced from the code itself. So, the depth of the tree, and as a
;; consequence, the space complexity of this program is related to the size of amount and
;; kinds-of-coins. However, 'kinds-of-coins' is a static value, so space complexity is 
;; dominated by 'amount' as it grows. Thus, the space complexity of this program is 
;; O(amount). 
;;
;; Time complexity is much harder to figure out, and I confess that I had to look up the 
;; answer myself. It turns out to be O(amount^kinds-of-coins). The way to see this is to
;; generate trees for cc with 1 kind of coin, then 2, then 3, and so on and notice how 
;; the branches (cc amount 1), (cc amount 2), (cc amount 3) etc. are repeated for each
;; increase in kinds-of-coins.