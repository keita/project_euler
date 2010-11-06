#!/usr/bin/env gosh
;; project euler: problem 5
;; Keita Yamaguchi
;; scheme version

(define (is_divisible n)
  (and (eq? (modulo n 20) 0)
       (eq? (modulo n 19) 0)
       (eq? (modulo n 18) 0)
       (eq? (modulo n 17) 0)
       (eq? (modulo n 16) 0)
       (eq? (modulo n 15) 0)
       (eq? (modulo n 14) 0)
       (eq? (modulo n 13) 0)
       (eq? (modulo n 12) 0)
       (eq? (modulo n 11) 0)))

(define (divisible_number n)
  (if (is_divisible n) n
      (divisible_number (+ n 20))))

(print "Answer: ")
(print (divisible_number 20))
