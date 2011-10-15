;; project euler: problem 18
;; Keita Yamaguchi

(use gauche.array)

(define height 15)
(define width 15)

(define data (make-array (shape 0 height 0 width) 0))

(define data-list
  '((75)
    (95 64)
    (17 47 82)
    (18 35 87 10)
    (20 04 82 47 65)
    (19 01 23 75 03 34)
    (88 02 77 73 07 63 67)
    (99 65 04 28 06 16 70 92)
    (41 41 26 56 83 40 80 70 33)
    (41 48 72 33 47 32 37 16 94 29)
    (53 71 44 65 25 43 91 52 97 51 14)
    (70 11 33 28 77 73 17 78 39 68 17 57)
    (91 71 52 38 17 14 91 43 58 50 27 29 48)
    (63 66 04 68 89 53 67 30 73 16 69 87 40 31)
    (04 62 98 27 23 09 70 98 73 93 38 53 60 04 23)))

(define (data->array2 l i j)
  (if (null? l) '()
      (begin
	(array-set! data i j (car l))
	(data->array2 (cdr l) i (+ j 1)))))

(define (data->array d i)
  (if (null? d) '()
      (begin (data->array2 (car d) i 0)
	     (data->array (cdr d) (+ i 1)))))

(define (resolve i j)
  (if (eq? i height) '() ; stop
      (let* ((val (array-ref data i j))
	     (r1 (resolve (+ i 1) j))
	     (r2 (resolve (+ i 1) (+ j 1))))
	(if (> (fold + 0 r1) (fold + 0 r2)) (cons val r1) (cons val r2)))))

(data->array data-list 0)

(display (resolve 0 0))
(newline)
(display (fold + 0 (resolve 0 0)))
(newline)
