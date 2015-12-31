;; project euler: problem 27
;; Keita Yamaguchi <keita.yamaguchi@gmail.com>
;;
;; scheme version solver:
;;   gosh problem_027.scm

(define (memoize f)
  (let* ((cache (make-hash-table 'equal?)))
    (define (memo x)
      (let ((res (f x)))
        (hash-table-put! cache x res) res))
    (lambda (x)
      (if (hash-table-exists? cache x)
          (hash-table-get cache x)
          (memo x)))))

(define prime?
  (memoize
   (lambda (n)
     (if (> n 1)
	 (let loop ((i 2))
	   (if (>= n (expt i 2))
	       (if (prime? i)
		   (if (= (modulo n i) 0)
		       #f
		       (loop (+ i 1)))
		   (loop (+ i 1)))
	       #t))
	 #f))))

(define (find-primes max n primes)
  (if (> max n)
      (find-primes max (+ n 1)
		   (if (prime? n) (cons n primes) primes))
      primes))

(define (quadratic-prime a b n) (+ (expt n 2) (* a n) b))

(define (length-of-primes a b)
  (let loop ((n 0))
      (if (prime? (quadratic-prime a b n))
	  (loop (+ n 1)) n)))

(define (find-longest-by-a a primes longest)
  (if (null? primes)
      longest
      (let* ((b (car primes))
	     (this-length (length-of-primes a b))
	     (longer? (> this-length (car longest))))
	(find-longest-by-a a (cdr primes)
			   (if longer? (list this-length a b) longest)))))

(define (solve range-a range-b)
  (let ((primes (find-primes range-b 2 '())))
    (let loop-a ((a (- 1 range-a)))
      (if (< a range-a)
	  (find-longest-by-a a primes (loop-a (+ a 1)))
	  (list 0)))))

(let ((longest (solve 1000 1000)))
  (print (list longest (fold * 1 (cdr longest)))))
