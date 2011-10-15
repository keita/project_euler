;; project euler: problem 45
;; Keita Yamaguchi

(load "memoize")

(define triangle_number
  (memoize (lambda (n) (quotient (* n (+ n 1)) 2))))
(define pentagonal_number
  (memoize (lambda (n) (quotient (* n (- (* 3 n) 1)) 2))))
(define (hexagonal_number n) (* n (- (* 2 n) 1)))

(define (is_triangle_number x n)
  (let ((num (triangle_number n)))
    ;; (print #`"T,|n| => ,|num|")
    (cond
     ((equal? x num) -1)
     ((> x num) (is_triangle_number x (+ n 1)))
     ((< x num) n))))

(define (is_pentagonal_number x n)
  (let ((num (pentagonal_number n)))
    ;; (print #`"P,|n| => ,|num|")
    (cond
     ((equal? x num) -1)
     ((> x num) (is_pentagonal_number x (+ n 1)))
     ((< x num) n))))

(define (find_answer n tn pn)
  (let*
      ((hnum (hexagonal_number n))
       (tnum (is_triangle_number hnum tn))
       (pnum (is_pentagonal_number hnum pn)))
    (print #`"H,|n| => ,|hnum|")
    (if (and (< tnum 0) (< pnum 0))
	hnum ; the answer
	(find_answer (+ n 1) tnum pnum))))

;(find_answer 2 0 0)
(find_answer 144 0 0)