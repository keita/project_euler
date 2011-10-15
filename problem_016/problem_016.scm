;; project euler: problem 16
;; Keita Yamaguchi

(define result
  (fold + 0 (map digit->integer (string->list (number->string (expt 2 1000))))))

(display "Answer: ")
(display result)
(newline)
