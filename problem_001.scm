;; project euler: problem 1
;; Keita Yamaguchi

(use srfi-1)

(define (mul i n)
  (if (< n 1000) (cons n (mul i (+ n i))) '()))
(define mul3 (mul 3 0))
(define mul5 (mul 5 0))
(define multiples (lset-union eq? mul3 mul5))

(display "multiples of 3 below 1000:\n")
(display mul3)
(newline)

(display "multiples of 5 below 1000:\n")
(display mul5)
(newline)

(display "multiples of 3 or 5 below 1000:\n")
(display multiples)
(newline)

(display "Answer: ")
(display (fold + 0 multiples))
(newline)