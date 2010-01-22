;; project euler: problem 14
;; Keita Yamaguchi

(define (get-chain n chain)
  (cond ((eq? n 1) '(1))
	((eq? n (car chain)) chain)
	((odd? n) (cons n (get-chain (+ (* 3 n) 1) chain)))
	((even? n) (cons n (get-chain (/ n 2) chain)))))

(define (find-longest-chain n chain)
  (if (>= n 1000000)
      (car chain)
      (find-longest-chain
       (+ n 1)
       (if (memq chain n)
	   chain
	   (let ((new-chain (get-chain n chain)))
	     (if (> (length new-chain) (length chain)) new-chain chain))))))

(display "Answer: ")
(display (find-longest-chain 1 '(1)))
(newline)
