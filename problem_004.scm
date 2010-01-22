;; project euler: problem 4
;; Keita Yamaguchi

(use srfi-1)

(define (is_palindromic n)
  (let ((nlist (string->list (number->string n))))
    (equal? nlist (reverse nlist))))

(define (palindrome n1 n2)
  (if (> 1000 n1)
      (append (palindrome2 n1 n2) (palindrome (+ n1 1) n2))
      '()))

(define (palindrome2 n1 n2)
  (if (> 1000 n2)
      (if (is_palindromic (* n1 n2))
	  (cons (* n1 n2) (palindrome2 n1 (+ n2 1)))
	  (palindrome2 n1 (+ n2 1)))
      '()))

(define palindromes (palindrome 100 100))

(display "palindromes made from product of two 3-digit numbers:\n")
(display palindromes)
(newline)

(display "Answer: ")
(display (fold max 0 palindromes))
(newline)
