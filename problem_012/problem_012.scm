;; project euler: problem 12
;; Keita Yamaguchi

(define (factors-of n i size limit)
  (if (eq? n 1) 1
      (if (>= i limit) size
	  (if (and (zero? (modulo n i)) (< i (/ n i)))
	      (factors-of n (+ i 1) (+ size 2) (/ n i))
	      (factors-of n (+ i 1) size limit)))))

(define (find-answer nth triangle-number)
  (let ((size (factors-of triangle-number 1 0 triangle-number)))
    (display #`"check ,nth nth (,triangle-number): ,size\n")
    (if (>= size 500)
	triangle-number
	(find-answer (+ nth 1) (+ triangle-number (+ nth 1))))))

(display "Answer: ")
(display (find-answer 1 1))
(newline)
