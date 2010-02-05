;; project euler: problem 21
;; Keita Yamaguchi

(define (memoize f)
  (let* ((cache (make-hash-table)))
    (define (memo x)
      (let1 res (f x) (hash-table-put! cache x res) res))
    (lambda (x)
      (if (hash-table-exists? cache x)
	  (hash-table-get cache x)
	  (memo x)))))

(define (proper-divisors n)
  (define f
    (memoize
     (lambda (i)
       (cond ((eq? i 1) 1)
	     ((and (eq? (modulo n i) 0) (not (eq? n i))) (+ i (f (- i 1))))
	     (else (f (- i 1)))))))
  (f n))

(define (find-amicable-pair n)
  (if (eq? n 0) '()
      (let* ((res1 (proper-divisors n))
	     (res2 (proper-divisors res1))
	     (prev (find-amicable-pair (- n 1))))
	(if (and (eq? n res2) (not (eq? n res1)) (not (memq res1 prev)))
	    (cons n (cons res1 prev))
	    prev))))

(define amicable-pair-under-10000 (find-amicable-pair 9999))
(display amicable-pair-under-10000)
(newline)
(display (fold + 0 amicable-pair-under-10000))
(newline)