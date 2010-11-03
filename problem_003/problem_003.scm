;; project euler: problem 3
;; Keita Yamaguchi

(use srfi-1)

(define (prime_factors_of n i)
  (cond ((eq? n 1) '())
	((= (modulo n i) 0) (cons i (prime_factors_of (/ n i) (+ i 1))))
	(else (prime_factors_of n (+ i 1)))))

(define result (prime_factors_of 600851475143 2))

(display "prime factors of 600851475143:\n")
(display result)
(newline)

(display "Answer: ")
(display (last result))
(newline)
