#lang planet neil/sicp

;; Exercise 2.24

(list 1 (list 2 (list 3 4)))

;; Returns: (mcons 1 (mcons (mcons 2 (mcons (mcons 3 (mcons 4 '())) '())) '()))