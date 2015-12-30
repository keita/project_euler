;; project euler: problem 35
;; Keita Yamaguchi <keita.yamaguchi@gmail.com>
;;
;; scheme version solver:
;;   gosh problem_035.scm

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
     (let loop ((i 2))
       (if (>= n (expt i 2))
	   (if (prime? i)
	       (if (= (modulo n i) 0)
		   #f
		   (loop (+ i 1)))
	       (loop (+ i 1)))
	   #t)))))

(define (make-circulars digits orig circulars)
  (let ((next-digits (reverse (cons (car digits) (reverse (cdr digits)))))
	(n (string->number (list->string digits))))
    (if (equal? next-digits orig)
	(cons n circulars)
	(make-circulars next-digits orig (cons n circulars)))))

(define circularable-chars (string->list "13579"))
(define (circularable? digit) (member digit circularable-chars))

(define (get-circular-primes n circular-primes)
  (if (member n circular-primes)
      circular-primes
      (let ((digits (string->list (number->string n))))
	(if (= (length digits) 1)
	    (if (prime? n) (cons n circular-primes) circular-primes)
	    (if (every circularable? digits)
		(let ((circulars (make-circulars digits digits '())))
		  (if (every prime? circulars)
		      (append circulars circular-primes)
		      circular-primes))
		circular-primes)))))

(define (find-circular-primes max n circular-primes)
  (if (< max n)
      circular-primes
      (find-circular-primes max (+ n 1)
			    (get-circular-primes n circular-primes))))

(define (solve max) (find-circular-primes max 2 '()))

;; (print (solve 1000000))
(print (length (solve 1000000)))

