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
(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (define (next n)
    (if (divides? 2 n)
	(+ n 1)
	(+ n 2)))
  (define (find-divisor test-divisor)
    (cond ((> (square test-divisor) n) n)
	  ((divides? test-divisor n) test-divisor)
	  (else (find-divisor (next test-divisor)))))
  (= n (find-divisor 2)))

(define l1 (list 1 2 3 4 5 6))
(define l2 (list 5 3 4 2 7 1 9 0 8))


(define (map proc l)
  (if (null? l)
      l
      (cons (proc (car l))
	    (map proc (cdr l)))))

(define (filter predicate sequence)
  (cond ((null? sequence) (list))
	((predicate (car sequence))
	 (cons (car sequence)
	       (filter predicate (cdr sequence))))
	(else
	 (filter predicate (cdr sequence)))))

(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1)
	    (append (cdr l1) l2))))

(define (accumulate op init l)
  (if (null? l)
      init
      (op (car l) (accumulate op init (cdr l)))))

(define (interval a b)
  (if (> a b)
      (list)
      (cons a (interval (+ a 1) b))))

(define (unique-pairs n)
  (accumulate append
	      (list)
	      (map (lambda (i)
		     (map (lambda (j) (list j i))
			  (interval 1 (- i 1))))	
		   (interval 1 n))))


(map (lambda (i)
       (map (lambda (j) (list j i))
	    (interval 1 (- i 1))))	
     (interval 1 5))
					; Evaluate this to see the list of lists

(define (flatmap l)
  (accumulate append (list) l))

(define (unique-pairs n)
  (flatmap (map (lambda (i)
		  (map (lambda (j) (list j i))
		       (interval 1 (- i 1))))	
		(interval 1 n))))

(define (prime-sum-pairs n)
  (define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair))))
  (define (make-pair-sum pair)
    (list (car pair) (cadr pair) (+ (car pair)
				    (cadr pair))))
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(define (ordered-triples n)
  (map (lambda (k)
	 (map (lambda (p)
		(append p (list k)))
	      (unique-pairs (- k 1))))
       (interval 3 n)))

(define (ordered-triples n)
  (map (lambda (k) (unique-pairs (- k 1)))
       (interval 3 n)))
