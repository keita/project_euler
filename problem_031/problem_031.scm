;; project euler: problem 31
;; Keita Yamaguchi <keita.yamaguchi@gmail.com>
;;
;; scheme version solver:
;;   gosh problem031.scm

(define (sum p1 p2 p5 p10 p20 p50 p100 p200)
  (+ p1 (* p2 2) (* p5 5) (* p10 10) (* p20 20) (* p50 50) (* p100 100) (* p200 200)))

(define (find-p1 n p1 p2 p5 p10 p20 p50 p100 p200 combs)
  (let ((x (sum p1 p2 p5 p10 p20 p50 p100 p200)))
    (cond ((= x n) (find-p1 n (+ p1 1) p2 p5 p10 p20 p50 p100 p200
			    (cons (list p1 p2 p5 p10 p20 p50 p100 p200) combs)))
	  ((< x n) (find-p1 n (+ p1 1) p2 p5 p10 p20 p50 p100 p200 combs))
	  ((> x n) combs))))

(define (find-p2 n p1 p2 p5 p10 p20 p50 p100 p200 combs)
  (let ((x (sum p1 p2 p5 p10 p20 p50 p100 p200)))
    (cond ((= x n) (find-p2 n p1 (+ p2 1) p5 p10 p20 p50 p100 p200
			    (cons (list p1 p2 p5 p10 p20 p50 p100 p200) combs)))
	  ((< x n) (find-p2 n p1 (+ p2 1) p5 p10 p20 p50 p100 p200
			    (find-p1 n p1 p2 p5 p10 p20 p50 p100 p200 combs)))
	  ((> x n) combs))))

(define (find-p5 n p1 p2 p5 p10 p20 p50 p100 p200 combs)
  (let ((x (sum p1 p2 p5 p10 p20 p50 p100 p200)))
    (cond ((= x n) (find-p5 n p1 p2 (+ p5 1) p10 p20 p50 p100 p200
			    (cons (list p1 p2 p5 p10 p20 p50 p100 p200) combs)))
	  ((< x n) (find-p5 n p1 p2 (+ p5 1) p10 p20 p50 p100 p200
			    (find-p2 n p1 p2 p5 p10 p20 p50 p100 p200 combs)))
	  ((> x n) combs))))

(define (find-p10 n p1 p2 p5 p10 p20 p50 p100 p200 combs)
  (let ((x (sum p1 p2 p5 p10 p20 p50 p100 p200)))
    (cond ((= x n) (find-p10 n p1 p2 p5 (+ p10 1) p20 p50 p100 p200
			     (cons (list p1 p2 p5 p10 p20 p50 p100 p200) combs)))
	  ((< x n) (find-p10 n p1 p2 p5 (+ p10 1) p20 p50 p100 p200
			     (find-p5 n p1 p2 p5 p10 p20 p50 p100 p200 combs)))
	  ((> x n) combs))))

(define (find-p20 n p1 p2 p5 p10 p20 p50 p100 p200 combs)
  (let ((x (sum p1 p2 p5 p10 p20 p50 p100 p200)))
    (cond ((= x n) (find-p20 n p1 p2 p5 p10 (+ p20 1) p50 p100 p200
			     (cons (list p1 p2 p5 p10 p20 p50 p100 p200) combs)))
	  ((< x n) (find-p20 n p1 p2 p5 p10 (+ p20 1) p50 p100 p200
			     (find-p10 n p1 p2 p5 p10 p20 p50 p100 p200 combs)))
	  ((> x n) combs))))

(define (find-p50 n p1 p2 p5 p10 p20 p50 p100 p200 combs)
  (let ((x (sum p1 p2 p5 p10 p20 p50 p100 p200)))
    (cond ((= x n) (find-p50 n p1 p2 p5 p10 p20 (+ p50 1) p100 p200
			     (cons (list p1 p2 p5 p10 p20 p50 p100 p200) combs)))
	  ((< x n) (find-p50 n p1 p2 p5 p10 p20 (+ p50 1) p100 p200
			     (find-p20 n p1 p2 p5 p10 p20 p50 p100 p200 combs)))
	  ((> x n) combs))))

(define (find-p100 n p1 p2 p5 p10 p20 p50 p100 p200 combs)
  (let ((x (sum p1 p2 p5 p10 p20 p50 p100 p200)))
    (cond ((= x n) (find-p100 n p1 p2 p5 p10 p20 p50 (+ p100 1) p200
			      (cons (list p1 p2 p5 p10 p20 p50 p100 p200) combs)))
	  ((< x n) (find-p100 n p1 p2 p5 p10 p20 p50 (+ p100 1) p200
			      (find-p50 n p1 p2 p5 p10 p20 p50 p100 p200 combs)))
	  ((> x n) combs))))

(define (find-p200 n p1 p2 p5 p10 p20 p50 p100 p200 combs)
  (let ((x (sum p1 p2 p5 p10 p20 p50 p100 p200)))
    (cond ((= x n) (find-p200 n p1 p2 p5 p10 p20 p50 p100 (+ p200 1)
			      (cons (list p1 p2 p5 p10 p20 p50 p100 p200) combs)))
	  ((< x n) (find-p200 n p1 p2 p5 p10 p20 p50 p100 (+ p200 1)
			      (find-p100 n p1 p2 p5 p10 p20 p50 p100 p200 combs)))
	  ((> x n) combs))))

(define (solve n) (find-p200 n 0 0 0 0 0 0 0 0 '()))

;; (let ((combs (solve 200)))
;;   (until (null? combs)
;; 	 (display (pop! combs))
;; 	 (newline)))

(display (length (solve 200)))
