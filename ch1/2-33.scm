(define (square x) (* x x))
(define (cube x) (* x x x))
(define (average a b) (/ (+ a b) 2))
(define (average3 a b c) (/ (+ a b c) 3))
(define (abs a) (if (< a 0) (- a) a))
(define (sign a) (if (< a 0) -1 1))
(define (div a b) (truncate (/ a b)))
(define (inc n) (+ n 1))

(define (<= a b) (not (> a b)))
(define (>= a b) (not (< a b)))
(define (!= a b) (not (= a b)))

(define (even? n) (= (remainder n 2) 0))
(define (same-parity? a b) (= (remainder a 2)
			      (remainder b 2)))

(define l1 (list 1 2 3 4 5 6))
(define l2 (list 5 3 4 2 7 1 9 0 8))


(define (map proc l)
  (if (null? l)
      l
      (cons (proc (car l))
	    (map proc (cdr l)))))

(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1)
	    (append (cdr l1) l2))))

(define (for-each proc items)
  (cond ((null? items)
	 true)
	(else
	 (proc (car items))
	 (for-each proc (cdr items)))))

(define (fringe t)
  (cond ((null? t)
	 (list))
	((not (pair? t))
	 (list t))
	(else
	 (append (fringe (car t))
		 (fringe (cdr t))))))

(define (accumulate op init l)
  (if (null? l)
      init
      (op (car l) (accumulate op init (cdr l)))))

(define (map p seq)
  (accumulate (lambda (x y) (cons (p x) y)) (list) seq))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1)) ; This works in all base cases! seq1 is empty, seq2 is empty, both are empty

(define (length seq)
  (accumulate (lambda (x y) (+ y 1)) 0 seq))

(define (horner-eval x coeff-seq)
  (accumulate (lambda (this-coeff higher-terms)
		(+ this-coeff (* x higher-terms)))
	      0
	      coeff-seq))
					; Look at the example list to convince yourself that x is multiplied to each coefficient the right amount of times. Also, notice how we only need to worry about the single accumulation operation - we don't have to worry about enumerating the sequence, or the underlying sequence's structure (in this case, a list).

(define ll1 (list (list 1 2 3)
		  (list 2 3 4)
		  (list 3 4 5)
		  (list 4 5 6)))

(define (accumulate-n op init seqs)
  (if (or (null? seqs) (null? (car seqs)))
      (list)
      (cons (accumulate op
			init
		        (map (lambda (x) (car x)) seqs))
	    (accumulate-n op
			  init
			  (map (lambda (x) (cdr x)) seqs)))))

(define  (dot-product v w)
  (accumulate + 0 (map * v w)))

(define v1 (list 1 2 3 4))
(define v2 (list -1 2 -1 1))

(define m1 (list (list 1 2 3 4)
		 (list 2 1 0 2)
		 (list 1 1 1 4)
		 (list 0 1 3 0)))

(define m2 (list (list 1 2 3 4)
		 (list 4 5 6 6)
		 (list 6 7 8 9)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (transpose mat)
  (accumulate-n cons (list) mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
	   (accumulate (lambda (col accum)
			 (cons (dot-product col row)
			       accum))
		       (list)
		       cols))
	 m)))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
	   (map (lambda (col)
		  (dot-product col row))
		cols))
	 m)))
; This solution is more clear

(define m3 (list (list 1 2 3)
		 (list 2 1 3)))
(define m4 (list (list 1 1)
		 (list 3 0)
		 (list 3 0)))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))
(define (fold-right op initial sequence)
  (accumulate op initial sequence))

(fold-right list (list) (list 1 2 3))
(fold-left list (list) (list 1 2 3))
(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-left cons (list) (list 1 2 3))
(fold-right cons (list) (list 1 2 3))
