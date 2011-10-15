;; project euler: problem 25
;; Keita Yamaguchi

(define (memoize f)
  (let ((cache (make-hash-table)))
    (define (memo x)
      (let1 res (f x) (hash-table-put! cache x res) res))
    (lambda (x)
      (if (hash-table-exists? cache x)
          (hash-table-get cache x)
          (memo x)))))

(define fib
  (memoize
   (lambda (n)
     (cond ((eq? n 1) 1)
	   ((eq? n 2) 1)
	   (else (+ (fib (- n 1)) (fib (- n 2))))))))

(define (resolve n)
  (if (eq? (string-length (number->string (fib n))) 1000) n
      (resolve (+ n 1))))

(display (resolve 1))
