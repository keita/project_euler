;; project euler: problem 10
;; Keita Yamaguchi

;; this method is very slow, fix it later

(define (is-prime n primes)
  (if (null? primes) #t
      (if (eq? (modulo n (car primes)) 0) #f
	  (is-prime n (cdr primes)))))

(define (prime n primes)
  (if (and (not (null? primes)) (< 2000000 (car primes)))
      (cdr primes)
      (if (is-prime n primes)
	  (begin (display n) (newline) (prime (+ n 1) (cons n primes)))
	  (prime (+ n 1) primes))))

(display "Answer: ")
(display (fold + 0 (prime 2 '()))) ; 142913828922
(newline)
