;; project euler: problem 29
;; Keita Yamaguchi <keita.yamaguchi@gmail.com>
;;
;; scheme version solver:
;;   gosh problem029.scm

(use gauche.sequence)

(define (get-terms a b terms)
  (let loop((nb 2) (new-terms terms))
    (if (<= nb b)
	(loop (+ nb 1) (cons (expt a nb) new-terms))
	new-terms)))

(define (get-all-terms a b)
  (let loop((na 2) (new-terms '()))
    (if (<= na a)
	(loop (+ na 1) (get-terms na b new-terms))
	new-terms)))

(define (solve a b)
  (length (map car (group-sequence (sort (get-all-terms a b))))))

(display (solve 100 100))
