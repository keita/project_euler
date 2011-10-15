;; project euler: problem 24
;; Keita Yamaguchi

(use srfi-1)

(define digits (iota 10))

(define (digit-list->integer s)
  (string->number (list->string (map integer->digit s))))

(define counter 0)
(define result '())

(define (make-permutations cur)
  (define (f d r) (make-permutations (cons d cur)))
  (if (eq? counter 1000000) result
      (if (eq? (length cur) 10)
	  (begin (set! counter (+ counter 1)) (set! result cur))
	  (fold f '() (lset-difference eq? digits cur)))))

(display (digit-list->integer (reverse (make-permutations '()))))
