#lang racket

;; Exercise 2.76

;; For explicit dispatch, any operation that must work on the new type of
;; data must be altered to include a case for the new data type. Simply 
;; adding a new type is straightforward, but incorporating it into the 
;; system can be cumbersome.

;; With a data-directed approach, a new type tag must be introduced for 
;; each new type. Additionally, each operation must register itself in the
;; dispatch table.

;; Adding new types in a message-passing system is the most involved, as
;; each new type will contain all of it's relevant operations and
;; dispatching. In turn, introducing new operations is straighforward, and
;; can be carried out independently of all other types.

;; If new types are often introduced, a data-directed approach is probably
;; best. A new type tag is all that is required. If new operations are
;; often added, a message-passing system is easier to work with. Explicit 
;; dispatch is cumbersome in both cases.