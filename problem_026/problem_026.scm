;; project euler: problem 26
;; Keita Yamaguchi

(use srfi-1)

(define (memoize f)
  (let ((cache (make-hash-table)))
    (define (memo x)
      (let1 res (f x) (hash-table-put! cache x res) res))
    (lambda (x)
      (if (hash-table-exists? cache x)
          (hash-table-get cache x)
          (memo x)))))

(define factors
  (memoize
   (lambda (n)
     (define (f i)
       (cond ((eq? (modulo n i) 0) i) ; i is a factor
	     ((> i (/ n 2)) n)        ; stop
	     (else (f (+ i 1)))))     ; go next
     (let1 factor (f 2)
	   (if (eq? factor n)
	       (cons factor '())
	       (cons factor (factors (quotient n factor))))))))

(define divisors
  (memoize
   (lambda (n)
     (define (f i)
       (let1 q (quotient n i)
	     (if (or (eq? (length (factors q)) 1) (eq? i 1))
		 (cons q '()) ; i is a prime number
		 (cons q (divisors q)))))
     (let1 fs (delete-duplicates (factors n))
	   (sort (cons n (delete-duplicates
		    (fold (lambda (ii l) (lset-union eq? (f ii) l)) '() fs))))))))

(define (repeating-type fs)
  (cond ((null? (lset-difference eq? fs '(2 5))) 'none)
	((or (memq fs 2) (memq fs 5) 'mixed))
	(else 'pure)))

(define (pure-cycle-length n)
  (define (f fs)
    (let1 i (car fs)
	  (if (eq? (modulo (expt 10 i) n) 1) i ; 10^i = 1 (mod n)
	      (f (cdr fs))))) ; else return prev
  (if (or (eq? n 0) (eq? n 1)) 0
      (f (divisors (- n 1)))))

(define (mixed-cycle-length n)
  (define (f i)
    (if (eq? (modulo (expt 10 i) n) 1) i ; 10^i = 1 (mod n)
	(f (+ i 1)))) ; else return prev
  (if (or (eq? n 0) (eq? n 1)) 0
      (f 2)))

(define (resolve n)
  (define (mixed fs) (mixed-cycle-length (fold * 1 (lset-difference eq? fs '(2 5)))))
  (let1 fs (factors n)
	(case (repeating-type fs)
	  ((none) 0)
	  ((mixed) (mixed fs))
	  ((pure) (pure-cycle-length n)))))


(define (find-answer n answer)
  (if (eq? n 1000) answer
      (let1 res (resolve n)
	    (find-answer (+ n 1) (if (> (car answer) res) answer (list res n))))))

(display (find-answer 1 '(0 0)))