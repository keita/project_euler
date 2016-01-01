;; project euler: problem 32
;; Keita Yamaguchi <keita.yamaguchi@gmail.com>
;;
;; scheme version solver:
;;   gosh problem_032.scm

(use gauche.sequence)

(define all-digits (iota 9 1))

(define possible-units
  (list
   '(1 4 4)
   '(2 3 4)))

(define (unique elements)
  (map car (group-sequence (sort elements))))

(define (choose size digits)
  (if (<= size 0)
      '(())
      (fold (lambda (n combs)
	     (append combs
		     (map (lambda (comb)
			    (cons n comb))
			  (choose (- size 1) (delete n digits)))))
	    '()
	    digits)))

(define (list->number list)
  (string->number
   (fold string-append ""
	 (reverse (map number->string list)))))

(define (pandigital-product? a b c)
  (= (* a b) c))

(define (find-pandigital-products size-a size-b size-c)
  (fold (lambda (a pandigital-products1)
	  (fold (lambda (b pandigital-products2)
		  (fold (lambda (c pandigital-products3)
			  (let ((na (list->number a))
				(nb (list->number b))
				(nc (list->number c)))
			    (if (pandigital-product? na nb nc)
				(cons (list na nb nc) pandigital-products3)
				pandigital-products3)))
			pandigital-products2
			(choose size-c (filter (lambda (digit)
						 (not (or (member digit a)
							  (member digit b))))
					       all-digits))))
		pandigital-products1
		(choose size-b (filter (lambda (digit)
					 (not (member digit a)))
				       all-digits))))
	'()
	(choose size-a all-digits)))

(define (solve units)
  (fold (lambda (unit pandigital-products)
	  (let ((size-a (list-ref unit 0))
		(size-b (list-ref unit 1))
		(size-c (list-ref unit 2)))
	    (append pandigital-products
		    (find-pandigital-products size-a size-b size-c))))
	'()
	units))

(let ((combs (solve possible-units)))
  (print combs)
  (print
   (fold + 0
	 (unique
	  (map
	   (lambda (comb) (list-ref comb 2))
	   combs)))))
