#!/usr/bin/env gosh
;; project euler: problem 1
;; scheme version
;; Keita Yamaguchi

(use srfi-1)

(define (mul i n)
  (if (< n 1000) (cons n (mul i (+ n i))) '()))
(define mul3 (mul 3 0))
(define mul5 (mul 5 0))
(define multiples (lset-union eq? mul3 mul5))

(print "multiples of 3 below 1000:")
(print mul3)

(print "multiples of 5 below 1000:")
(print mul5)

(print "multiples of 3 or 5 below 1000:")
(print multiples)

(print "Answer: ")
(print (fold + 0 multiples))
