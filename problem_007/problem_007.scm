#!/usr/bin/env gosh
;; project euler: problem 7
;; Keita Yamaguchi
;; scheme version

(define (is_prime n primes)
  (if (null? primes) #t
      (if (eq? (modulo n (car primes)) 0) #f
	  (is_prime n (cdr primes)))))

(define (prime n primes)
  (if (eq? (length primes) 10001) (- n 1)
      (if (is_prime n primes)
	  (prime (+ n 1) (cons n primes))
	  (prime (+ n 1) primes))))

(print (prime 2 '()))
