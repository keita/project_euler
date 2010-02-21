;; project euler: problem 265
;; Keita Yamaguchi

(use srfi-1)
(use srfi-13)

(define (number->bstring size i) (string-pad (number->string i 2) size #\0))
(define (make-elements size)
  (map (pa$ number->bstring size) (iota (expt 2 size))))

(define (chain->number chain) (string->number (chain->string chain) 2))
(define (chain->string chain)
  (if (null? chain) ""
      (string-append (substring (car chain) 0 1) (chain->string (cdr chain)))))

(define (next? a b)
  (let1 len (string-length a)
	(and (eq? (string-length b) len)
	     (not (equal? a b))
	     (equal? (substring a 1 len) (substring b 0 (- len 1))))))

(define (next-numbers nums s)
  (filter (pa$ next? s) nums))

(define (find-chains acc nums s res)
  (let ((nums2 (remove (pa$ equal? s) nums))
	(next (next-numbers nums s)))
    (cond ((equal? nums (list s)) (cons (reverse (cons s acc)) res)) ; found a chain
	  ((null? next) res) ; break chain
	  (else (fold (pa$ find-chains (cons s acc) nums2) res next))))) ; keep to find chains

(define (find-chained-strings elts)
  (map chain->string  (find-chains '() elts (car elts) '())))

(define (resolve elts)
  (fold + 0 (map chain->number (find-chains '() elts (car elts) '()))))

(define elts3 (make-elements 3))
(define elts5 (make-elements 5))

;(print elts3)
;(print elts5)

;; for testing chain conversions
;(print (chain->string '("000" "001" "010" "101" "011" "111" "110" "100")))
;(print (chain->number '("000" "001" "010" "101" "011" "111" "110" "100")))

;(print (map (pa$ next-numbers elts5) elts5))
(print "result:")
;(print (find-chains '() elts3 "000" '()))
;(print (find-chained-strings elts3))
(print (resolve elts5))