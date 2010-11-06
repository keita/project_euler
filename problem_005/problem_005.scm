;; project euler: problem 5
;; Keita Yamaguchi

(define (is_dividible n)
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

(define (dividible_number n)
  (if (is_dividible n) n
      (dividible_number (+ n 20))))

(display "Answer: ")
(display (dividible_number 20))
(newline)
