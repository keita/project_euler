#!/usr/bin/env gosh
;; project euler: problem 006
;; Keita Yamaguchi
;; scheme version

(define (sum_of_squares_of n)
  (if (> n 0) (+ (* n n) (sum_of_squares_of (- n 1))) 0))

(define (square_of_sum_of n)
  (let1 sum (* (/ (+ 1 n) 2) n) (* sum sum)))

(print "Answer: ")
(print (- (square_of_sum_of 100) (sum_of_squares_of 100)))
