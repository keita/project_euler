;; project euler: problem 277
;; Keita Yamaguchi

(use srfi-1)

(define (memoize f)
  (let* ((cache (make-hash-table)))
    (define (memo x)
      (let1 res (f x) (hash-table-put! cache x res) res))
    (lambda (x)
      (if (hash-table-exists? cache x)
	  (hash-table-get cache x)
	  (memo x)))))

(define seq-test1 (string->list "DdDddUUdDD"))
(define seq-test2 (string->list "DdDddUUdDDDdUDUUUdDdUUDDDUdDD"))

(define partial-seq (string->list "UDDDUdddDDUDDddDdDddDDUDDdUUDd"))

(define (D n) (* n 3))
(define (U n) (quotient (- (* n 3) 2) 4))
(define (d n) (quotient (+ (* n 3) 1) 2))
(define (calc c n)
  (case c ((#\D) (D n)) ((#\U) (U n)) ((#\d) (d n))))

;;
;; from seq to number
;;
(define (rseq->number seq n)
  (if (null? seq) n
      (rseq->number (cdr seq) (calc (car seq) n))))

(define (seq->number seq) (rseq->number (reverse seq) 1))

;;
;; from number to seq
;;
(define number->seq
  (memoize
   (lambda (n)
     (if (eq? n 1) '()
	 (case (modulo n 3)
	   ((0) (cons #\D (number->seq (quotient n 3))))
	   ((1) (cons #\U (number->seq (quotient (+ (* n 4) 2) 3))))
	   ((2) (cons #\d (number->seq (quotient (- (* n 2) 1) 3)))))))))

(define (subseq? subseq n)
  (if (null? subseq) #t
      (let ((hd (car subseq)) (tl (cdr subseq)))
	(case (modulo n 3)
	  ((0) (if (eq? #\D hd) (subseq? tl (quotient n 3)) #f))
	  ((1) (if (eq? #\U hd) (subseq? tl (quotient (+ (* n 4) 2) 3)) #f))
	  ((2) (if (eq? #\d hd) (subseq? tl (quotient (- (* n 2) 1) 3)) #f))))))


(define (find-answer subseq n)
  (if (subseq? subseq n) n
      (find-answer subseq (+ n 1))))

(define (resolve subseq min) (find-answer subseq min))

;(print (seq->number partial-seq))
;(print (number->seq 231))
(print (resolve partial-seq 1000000000000000))
;(print (resolve (string->list "DdDddUUdDD") 1000000))
