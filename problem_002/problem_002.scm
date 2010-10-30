#!/usr/bin/env gosh
;; project euler: problem 2
;; Keita Yamaguchi

(use srfi-1)

(define (fib n1 n2)
  (cond ((< n2 4000000) (cons n1 (fib n2 (+ n1 n2))))
	(else (cons n1 '()))))

(define fib4m (fib 1 2))
(define fib4m_even (filter even? fib4m))

(print "fib sequence below 4m:")
(print fib4m)

(print "even numbers of the fib sequence below 4m:")
(print fib4m_even)

(print "Answer: ")
(print (fold + 0 fib4m_even))
