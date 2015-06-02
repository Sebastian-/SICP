#lang racket

;; Exercise 2.74

;; The structure of the system is something like this:

;;--------------Headquarters-----------------
;; get-record get-salary find-employee-record

;; -------------Divisions--------------------
;; Division 1       |  Division 2      |  etc...
;; Personnel record | Personnel record |
;;                  |                  |
;; get 'employee    | get 'employee    |
;;                  |                  |
;; ----------Employee Records----------------
;; get 'address     | get 'address     |
;; get 'salary      | get 'salary      |

;; Part a

;; Each division should structure their files in the following way:
;; Tag the personnel record with the division name
;;     Eg. (Tag 'division-1 personnel-record)
;; Put, in a dispatch table, their implementation of retrieving employee
;; records using thier division name as a key.
;;     Eg. (define (get-employee-record employee) ...)
;;         (put 'division-1 get-employee-record)

(define (get-record employee personnel-record)
  ((get (get-tag personnel-record))       ; get function implementation
   employee (get-data personnel-record))) ; call on employee key

;; Part b

;; Again we have to tag the employee records in order to determine which 
;; get-salary implementation we should call

(define (get-salary employee-record)
  ((get (get-tag employee-record))        ; get function implementation
   'salary (get-data employee-record)))   ; call on 'salary

;; Part c

(define (find-employee-record employee list-of-division-files)
  (let ([employee-record 
         (get-record employee (car list-of-division-files))])   
    (if (not-found? employee-record)
        (find-employee-record employee (cdr list-of-division-files))
        employee-record)))

;; Part d

;; The new company must add their implementation to the dispatch table 
;; with type names different from those already in the table


