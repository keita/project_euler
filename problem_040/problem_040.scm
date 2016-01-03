;; project euler: problem 40
;; Keita Yamaguchi <keita.yamaguchi@gmail.com>
;;
;; scheme version solver:
;;   gosh problem_040.scm

(define (memoize f)
  (let ((cache (make-hash-table 'equal?)))
    (define (memo x)
      (let ((res (f x)))
        (hash-table-put! cache x res) res))
    (lambda (x)
      (if (hash-table-exists? cache x)
          (hash-table-get cache x)
          (memo x)))))

(define length-of-integer
  (memoize
   (lambda (n)
     (if (< n 1) 0
	 (+ (length-of-integer (- n 1))
	    (string-length (number->string n)))))))

(define (find-dn n a)
  (let ((index (length-of-integer a))
	(prev-index (length-of-integer (- a 1))))
    (if (>= index n)
	(digit->integer (list-ref (string->list (number->string a)) (- n prev-index 1)))
	(find-dn n (+ a 1)))))

(define (solve n) (find-dn n 1))

(let* ((d1 (solve 1))
       (d10 (solve 10))
       (d100 (solve 100))
       (d1000 (solve 1000))
       (d10000 (solve 10000))
       (d100000 (solve 100000))
       (d1000000 (solve 1000000))
       (ds (list d1 d10 d100 d1000 d10000 d100000 d1000000)))
  (print ds)
  (print (fold * 1 ds)))
