;; project euler: problem 34
;; Keita Yamaguchi <keita.yamaguchi@gmail.com>
;;
;; scheme version solver:
;;   gosh problem_034.scm

(define (memoize f)
  (let ((cache (make-hash-table 'equal?)))
    (define (memo x)
      (let ((res (f x)))
        (hash-table-put! cache x res) res))
    (lambda (x)
      (if (hash-table-exists? cache x)
          (hash-table-get cache x)
          (memo x)))))

(define factorial
  (memoize
   (lambda (n)
     (if (<= n 1)
	 1
	 (* n (factorial (- n 1)))))))

(define (exists-curious-numbers? size)
  (let ((max (factorial 9)))
    (>= (* max size) (expt 10 size))))

(define (curious-number? n)
  (= n (fold + 0
	     (map (lambda (digit)
		    (factorial (digit->integer digit)))
		  (string->list (number->string n))))))

(define (find-curious-numbers i curious-numbers)
  (if (exists-curious-numbers? (string-length (number->string i)))
      (find-curious-numbers (+ i 1)
			    (if (curious-number? i)
				(cons i curious-numbers)
				curious-numbers))
      curious-numbers))

(define (solve start) (find-curious-numbers start '()))

(let ((curious-numbers (solve 3)))
  (print curious-numbers)
  (print (fold + 0 curious-numbers)))
