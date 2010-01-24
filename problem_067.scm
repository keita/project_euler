;; project euler: problem 67
;; Keita Yamaguchi

(use gauche.array)

(define height 100)
(define width 100)

(define data (make-array (shape 0 height 0 width) 0))

(define data-list
  (map (lambda (s) (map string->number (string-split s " ")))
       (call-with-input-file "problem_067/triangle.txt"
	 (lambda (port) (port->string-list port)))))

(define (data->array2 l i j)
  (if (null? l) '()
      (begin
	(array-set! data i j (car l))
	(data->array2 (cdr l) i (+ j 1)))))

(define (data->array d i)
  (if (null? d) '()
      (begin (data->array2 (car d) i 0)
	     (data->array (cdr d) (+ i 1)))))

(define (memoize f)
  (let* ((cache (make-hash-table 'equal?)))
    (define (memo x y)
      (let ((res (f x y)))
	(hash-table-put! cache (list x y) res) res))
    (lambda (x y)
      (if (hash-table-exists? cache (list x y))
	  (hash-table-get cache (list x y))
	  (memo x y)))))

(define resolve
  (memoize
   (lambda (i j)
     (if (eq? i height) '() ; stop
	 (let* ((val (array-ref data i j))
		(r1 (resolve (+ i 1) j))
		(r2 (resolve (+ i 1) (+ j 1))))
	   (if (> (fold + 0 r1) (fold + 0 r2)) (cons val r1) (cons val r2)))))))

(data->array data-list 0)

(display (resolve 0 0))
(newline)
(display (fold + 0 (resolve 0 0)))
(newline)
