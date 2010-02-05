;; project euler: problem 20
;; Keita Yamaguchi

(define (factorial n)
  (if (eq? n 1) 1
      (* n (factorial (- n 1)))))

(display
 (fold + 0 (map digit->integer (string->list (number->string (factorial 100))))))
(newline)
