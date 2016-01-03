;; project euler: problem 37
;; Keita Yamaguchi <keita.yamaguchi@gmail.com>
;;
;; scheme version solver:
;;   gosh problem_037.scm

(define (pandigital? size n)
  (equal? (sort (string->list (number->string n)))
	  (map integer->digit (iota size 1))))

(define (concatenated-product n max)
  (string->number
   (fold string-append ""
	 (let loop ((i 1) (numbers '()))
	   (if (<= i max)
	       (loop (+ i 1) (cons (number->string (* n i)) numbers))
	       numbers)))))

(define (pandigital-multiple? n max)
    (pandigital? 9 (pandigital-multiple n max)))

(define (find-pandigitals n digit-size max pandigitals)
  (if (< n (expt 10 digit-size))
      (let ((i (concatenated-product n max)))
	(find-pandigitals (+ n 1) digit-size max
			  (if (pandigital? 9 i)
			      (cons i pandigitals)
			      pandigitals)))
      pandigitals))

(define (solve pairs)
  (if (null? pairs)
      '()
      (let* ((pair (car pairs))
	     (digit-size (car pair))
	     (max (car (cdr pair)))
	     (start (+ (expt 10 (- digit-size 1)) 1)))
	(find-pandigitals start digit-size max (solve (cdr pairs))))))

(define possible-pairs
  '((1 9)
    (3 3)
    (4 2)))

(let ((pandigitals (solve possible-pairs)))
  (print pandigitals)
  (print (fold max 0 pandigitals)))
