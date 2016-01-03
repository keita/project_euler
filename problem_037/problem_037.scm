;; project euler: problem 37
;; Keita Yamaguchi <keita.yamaguchi@gmail.com>
;;
;; scheme version solver:
;;   gosh problem_037.scm

(define (memoize f)
  (let ((cache (make-hash-table 'equal?)))
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

(define (digits->number digits)
  (string->number (list->string digits)))

(define (truncate-from-left n)
  (filter number?
	  (map digits->number
	       (fold (lambda (digit numbers)
		       (cons (cons digit (car numbers)) numbers))
		     '(())
		     (reverse (string->list (number->string n)))))))

(define (truncate-from-right n)
  (filter number?
	  (map digits->number
	       (fold (lambda (digit numbers)
		       (cons (reverse (cons digit (reverse (car numbers)))) numbers))
		     '(())
		     (string->list (number->string n))))))

(define (truncatable-prime? n)
  (if (and (> n 9) (prime? n))
      (and (every prime? (truncate-from-left n))
	   (every prime? (truncate-from-right n)))
      #f))

(define (find-truncatable-primes rest n truncatable-primes)
  (if (> rest 0)
      (let ((is-truncatable-prime (truncatable-prime? n)))
	(find-truncatable-primes (if is-truncatable-prime (- rest 1) rest)
				 (+ n 1)
				 (if is-truncatable-prime
				     (cons n truncatable-primes)
				     truncatable-primes)))
      truncatable-primes))

(define (solve rest) (find-truncatable-primes rest 2'()))

(let ((truncatable-primes (solve 11)))
  (print truncatable-primes)
  (print (fold + 0 truncatable-primes)))
