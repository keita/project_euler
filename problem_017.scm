;; project euler: problem 17
;; Keita Yamaguchi

(use srfi-1)

(define lexicon (make-hash-table))

(define words
  '((0 . "zero")
    (1 . "one")
    (2 . "two")
    (3 . "three")
    (4 . "four")
    (5 . "five")
    (6 . "six")
    (7 . "seven")
    (8 . "eight")
    (9 . "nine")
    (10 . "ten")
    (11 . "eleven")
    (12 . "twelve")
    (13 . "thirteen")
    (14 . "fourteen")
    (15 . "fifteen")
    (16 . "sixteen")
    (17 . "seventeen")
    (18 . "eighteen")
    (19 . "nineteen")
    (20 . "twenty")
    (30 . "thirty")
    (40 . "forty")
    (50 . "fifty")
    (60 . "sixty")
    (70 . "seventy")
    (80 . "eighty")
    (90 . "ninety")
    (100 . "hundred")
    (1000 . "thousand")))

(define (set-words words)
  (if (null? words) '()
      (let ((pair (car words)))
	(hash-table-put! lexicon (car pair) (cdr pair))
	(set-words (cdr words)))))

(set-words words)

(define (takeXX n)
  (string->number (list->string (reverse (take (reverse (string->list (number->string n))) 2)))))

(define (digit1 n)
  (digit->integer (list-ref (reverse (string->list (number->string n))) 0)))

(define (digit10 n)
  (digit->integer (list-ref (reverse (string->list (number->string n))) 1)))

(define (count10 n)
  (if (< n 10)
      (hash-table-get lexicon n)
      (let ((d10 (digit10 n))
	    (d1 (digit1 n))
	    (xx (takeXX n)))
	(if (hash-table-exists? lexicon xx)
	    (hash-table-get lexicon xx)
	    (string-append (hash-table-get lexicon (* d10 10)) (hash-table-get lexicon d1))))))

(define (digit100 n)
  (digit->integer (list-ref (reverse (string->list (number->string n))) 2)))

(define (count100 n)
  (if (< n 100) ""
      (let ((d (digit100 n)))
	(string-append (hash-table-get lexicon d) (hash-table-get lexicon 100)))))

(define (digit1000 n)
  (digit->integer (list-ref (reverse (string->list (number->string n))) 3)))

(define (count1000 n)
  (if (< n 1000) ""
      (let ((d (digit1000 n)))
	(string-append (hash-table-get lexicon d) (hash-table-get lexicon 1000)))))

(define (count n)
  (cond ((< n 100) (count10 n))
	((or (= n 100) (and (< n 1000) (zero? (modulo n 100)))) (count100 n))
	((< n 1000) (string-append (count100 n) "and" (count10 n)))
	(else (count1000 n))))

(define (count-with-print n)
  (let1 res (count n) (display res) (newline) res))

(define (count-up n)
  (if (> n 1000) "" (string-append (count-with-print n) (count-up (+ n 1)))))

(display (string-length (count-up 1)))
(newline)
