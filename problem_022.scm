;; project euler: problem 22
;; Keita Yamaguchi

(define (unquote-name name)
  (let1 len (string-length name) (string-copy name 1 (- len 1))))

(define names
  (sort (call-with-input-file "problem_022/names.txt"
	  (lambda (port)
	    (let1 s (port->string port) (map unquote-name (string-split s ",")))))))

(define (char-score c) (- (char->integer c) 64))

(define (name-a-score name)
  (fold + 0 (map char-score (string->list name))))

(define (sum-of-scores names i)
  (if (null? names) 0
      (begin (display #`",(car names) ,i\n")
	(+ (* i (name-a-score (car names))) (sum-of-scores (cdr names) (+ i 1))))))

(display (sum-of-scores names 1))
(newline)