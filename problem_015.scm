;; project euler: problem 15
;; Keita Yamaguchi

(define (memoize f)
  (let* ((cache (make-hash-table 'equal?)))
    (define (memo x y)
      (let ((res (f x y)))
	(display #`"memo! x:,x y:,y routes:,res\n")
	(hash-table-put! cache (list x y) res) res))
    (lambda (x y)
      (if (hash-table-exists? cache (list x y))
	  (hash-table-get cache (list x y))
	  (memo x y)))))

(define routes
  (memoize
   (lambda (x y)
     (if (or (eq? x 0) (eq? y 0)) 1
	 (+ (routes (- x 1) y) (routes x (- y 1)))))))

;;(display "Answer: ")
(display (routes 20 20))
(newline)


