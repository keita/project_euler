;; project euler: problem 28
;; Keita Yamaguchi <keita.yamaguchi@gmail.com>
;;
;; scheme version
;; gosh problem028.scm

(define (memoize f)
  (let* ((cache (make-hash-table 'equal?)))
    (define (memo x)
      (let ((res (f x)))
        (hash-table-put! cache x res) res))
    (lambda (x)
      (if (hash-table-exists? cache x)
          (hash-table-get cache x)
          (memo x)))))

(define spiral-diagonals
  (lambda (n)
    (if (eq? n 1)
	1
	(+ (spiral-diagonals (- n 2))
	   (- (* 4 (expt n 2)) (* 6 (- n 1)))))))

(display (spiral-diagonals 1001))
