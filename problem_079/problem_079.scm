;; project euler: problem 79
;; Keita Yamaguchi

(use gauche.sequence)

; get keys from log
(define keys (port->string-list (open-input-file "problem_079/keylog.txt")))

; split key
; key's format is 'abc', make three 2-digits numbers like 'ab', 'bc', and 'ac'
(define (split-key key)
  (let*
      ((chars (string->list key))
       (c0 (digit->integer (list-ref chars 0)))
       (c1 (digit->integer (list-ref chars 1)))
       (c2 (digit->integer (list-ref chars 2))))
    (list (cons c0 c1) (cons c1 c2) (cons c0 c2))))

; flatten list
(define (flatten tokens)
  (fold (lambda (token res)
	  (let
	      ((t0 (list-ref token 0))
	       (t1 (list-ref token 1))
	       (t2 (list-ref token 2)))
	    (cons t2 (cons t1 (cons t0 res))))) '() tokens))

; compare function for pairs
(define (compare-pair a b)
  (if (= (car a) (car b)) (< (cdr a) (cdr b)) (< (car a) (car b))))

; unique
(define (unique list cmpfun) (map car (group-sequence (sort list cmpfun) :test equal?)))

; get pairs
(define pairs (unique (flatten (map split-key keys)) compare-pair))

(print "all pairs from keys:")
(print pairs)

; find all digits
(define (find-digits pairs)
  (fold (lambda (pair res) (cons (car pair) (cons (cdr pair) res))) '() pairs))

(define digits (unique (find-digits pairs) (lambda (a b) (< a b))))

(print "all digits:")
(print (sort digits object-compare))

(define answer (sort digits (lambda (a b) (member (cons a b) pairs))))

(print "answer:")
(print (list->string (map integer->digit answer)))
