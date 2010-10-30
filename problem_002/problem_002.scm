;; project euler: problem 2
;; Keita Yamaguchi

(use srfi-1)

(define (fib n1 n2)
  (cond ((< n2 4000000) (cons n1 (fib n2 (+ n1 n2))))
	(else (cons n1 '()))))

(define fib4m (fib 1 2))
(define fib4m_even (filter even? fib4m))

(display "fib sequence below 4m:\n")
(display fib4m)
(newline)

(display "even numbers of the fib sequence below 4m:\n")
(display fib4m_even)
(newline)

(display "Answer: ")
(display (fold + 0 fib4m_even))
(newline)
