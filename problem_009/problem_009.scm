;; project euler: problem 9
;; Keita Yamaguchi

(define (is-Pythagorean-triplet a b c)
  (and (< a b)
       (< b c)
       (eq? (+ (* a a) (* b b)) (* c c))))

(define (find-Pythagorean2 a c)
  (let ((b (- 1000 a c)))
    (if (< a b)
	(if (is-Pythagorean-triplet a b c)
	    (* a b c)
	    (find-Pythagorean2 (+ a 1) c))
	0)))

(define (find-Pythagorean c)
  (if (> 1000 c)
      (let1 pproduct (find-Pythagorean2 1 c)
	    (if (> pproduct 0) pproduct (find-Pythagorean (+ c 1))))))

(print (find-Pythagorean 2))
