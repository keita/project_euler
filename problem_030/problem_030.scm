;; project euler: problem 35
;; Keita Yamaguchi <keita.yamaguchi@gmail.com>
;;
;; scheme version solver:
;;   gosh problem_035.scm

(define (memoize f)
  (let* ((cache (make-hash-table 'equal?)))
    (define (memo x y)
      (let ((res (f x y)))
        (hash-table-put! cache (list x y) res) res))
    (lambda (x y)
      (if (hash-table-exists? cache (list x y))
          (hash-table-get cache (list x y))
          (memo x y)))))

(define nth-power
  (memoize (lambda (digit nth) (expt digit nth))))

(define (match-digits-powers? number power)
  (let ((digits (string->list (number->string number))))
    (= number
       (fold + 0 (map (lambda (digit)
			(expt (digit->integer digit) power))
		      digits)))))

(define (find-answers max n power numbers)
  (if (> max n)
      (find-answers max (+ n 1) power
		    (if (match-digits-powers? n power)
			(cons n numbers)
			numbers))
      numbers))

(define (find-by-digits-size digits-size power numbers)
  (if (< (expt 10 digits-size) (* (nth-power 9 power) digits-size))
      (find-answers (- (expt 10 (+ digits-size 1)) 1)
		    (expt 10 digits-size)
		    power
		    (find-by-digits-size (+ digits-size 1) power numbers))
      numbers))

(define (solve power) (find-by-digits-size 2 power '()))

(print (fold + 0 (solve 5)))
