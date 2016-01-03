;; project euler: problem 39
;; Keita Yamaguchi <keita.yamaguchi@gmail.com>
;;
;; scheme version solver:
;;   gosh problem_039.scm

(define (integer-right-triangle? a b c)
  (= (+ (expt a 2) (expt b 2)) (expt c 2)))

(define (find-triangles-by-b sum a b triangles)
  (if (and (>= a b) (< (+ a b) sum))
      (let* ((c (- sum a b))
	     (triangle (sort (list a b c))))
	(find-triangles-by-b sum a (+ b 1)
			     (if (and (not (member triangle triangles))
				      (integer-right-triangle? a b c))
				 (cons triangle triangles)
				 triangles)))
      triangles))

(define (find-triangles-by-a max sum a triangles)
  (if (< a sum)
      (find-triangles-by-a max sum (+ a 1)
			   (find-triangles-by-b sum a 1 triangles))
      triangles))

(define (find-triangles max sum combs)
  (if (<= sum max)
      (find-triangles max (+ sum 1)
		      (cons (find-triangles-by-a max sum 1 '()) combs))
      combs))

(define (solve max) (find-triangles max 3 '()))

(let ((combs (solve 1000)))
  (print combs)
  (print (fold + 0
	       (car
		(fold (lambda (triangles max-triangles)
			(if (> (length triangles) (length max-triangles))
			    triangles
			    max-triangles))
		      '() combs)))))
