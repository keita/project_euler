;; project euler: problem 19
;; Keita Yamaguchi

;; this is a unfair method

(use srfi-19)

(define (make-mydate year month)
  (make-date 0 0 0 0 1 month year 0))

(define (count y m)
  (cond ((eq? y 2001) 0)
	((eq? m 13) (count (+ y 1) 1))
	(else
	 (let ((res (if (eq? (date-week-day (make-mydate y m)) 0) 1 0)))
	   (+ res (count y (+ m 1)))))))

(print (count 1901 1))
