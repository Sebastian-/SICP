#lang planet neil/sicp

;; Exercise 2.53

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(list 'a 'b 'c)
;; (a b c)

(list (list 'george))
;; ((george))

(cdr '((x1 x2) (y1 y2)))
;; ((y1 y2))

(cadr '((x1 x2) (y1 y2)))
(car (cdr '((x1 x2) (y1 y2))))
;; (y1 y2)

(pair? (car '(a short list)))
;; false

(memq 'red '((red shoes) (blue socks)))
;; false, memq is not tree recursive (i.e. it doesn't search the lists 
;; inside the primary list)

(memq 'red '(red shoes blue socks))
;; (red shoes blue socks)

