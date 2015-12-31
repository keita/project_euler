;; project euler: problem 33
;; Keita Yamaguchi <keita.yamaguchi@gmail.com>
;;
;; scheme version solver:
;;   gosh problem_033.scm

(define (cancel-fraction n i)
  (let ((cancelled
	 (filter (lambda (digit)
		   (not (equal? digit (car (string->list (number->string i))))))
		 (string->list (number->string n)))))
    (if (> (length cancelled) 0)
	(string->number (list->string cancelled)) 0)))

(define (match-cancelling-fraction? a b)
  (any
   (lambda (i)
     (let ((cancelled-a (cancel-fraction a i))
	   (cancelled-b (cancel-fraction b i)))
       (if (or (= cancelled-a a) (= cancelled-b b) (= cancelled-b 0))
	   #f
	   (= (/ a b) (/ cancelled-a cancelled-b)))))
   (iota 9 1)))

(define (make-pairs number-range)
  (fold (lambda (a pairs1)
	  (fold (lambda (b pairs2)
		  (if (< a b) (cons (cons a b) pairs2) pairs2))
		pairs1 number-range))
	'() number-range))

(define (find-cancelling-fraction-pairs number-range)
  (filter (lambda (pair)
	    (match-cancelling-fraction? (car pair) (cdr pair)))
	  (make-pairs number-range)))

(define (solve number-range)
  (let ((pairs (find-cancelling-fraction-pairs number-range)))
    (fold * 1 (map (lambda (pair) (/ (car pair) (cdr pair))) pairs))))

(print (solve (iota 89 10)))
