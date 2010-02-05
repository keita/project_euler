;; project euler: problem 23
;; Keita Yamaguchi

(define (sum-of-proper-divisors n)
  (define (f i max)
    (cond ((eq? i 1) (+ 1 (f (+ i 1) (- n 1))))
	  ((>= i max) 0)
	  ((eq? (modulo n i) 0)
	   (let1 j (quotient n i)
		 (if (eq? i j) i (+ i j (f (+ i 1) j)))))
	  (else (f (+ i 1) max))))
  (f 1 n))

(define (find-abundant-numbers n l)
  (if (eq? n 0) l
      (find-abundant-numbers
       (- n 1)
       (if (> (sum-of-proper-divisors n) n) (cons n l) l))))

(define abundant-numbers (find-abundant-numbers 28122 '()))

(define (resolve n sum)
  (define (f l)
    (if (or (null? l) (< n (car l))) #t
	(let1 m (- n (car l))
	      (if (memq m l) #f (f (cdr l))))))
  (display #`"resolve ,n\n")
  (if (eq? n 0) sum
      (resolve (- n 1) (if (f abundant-numbers) (+ n sum) sum))))

;(display abundant-numbers)
(display "Answer: ")
(display (resolve 28122 0))
(newline)