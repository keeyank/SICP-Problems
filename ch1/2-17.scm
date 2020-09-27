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

(define (last-pair l)
  (if (null? (cdr l))
      (car l)
      (last-pair (cdr l))))

(define (reverse l)
  (define (rev old new)
    (if (null? old)
	new
	(rev (cdr old) (cons (car old) new))))
  (rev l (list)))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (null? kinds-of-coins)) 0)
        (else (+ (cc amount
                     (cdr kinds-of-coins))
                 (cc (- amount
                        (car kinds-of-coins))
                     kinds-of-coins)))))

(define us-coins (list 50 25 10 5 1))
					; (cc 100 us-coins) is equal to (cc 100 (reverse us-coins)). Order of the list doesn't matter! This is because the tree recursion makes it so every possible combination of coin types is selected, no matter the order of the coins in the list. The fact that we add the cc call with the reduced kinds-of-coins list to the cc call with the reduced amount ensures this.


(define (same-parity first . l)
  (let ((p (remainder first 2)))
    (define (sp l)
      (cond ((null? l)
	     l)
	    ((same-parity? (car l) p)
	     (cons (car l) (sp (cdr l))))
	    (else
	     (sp (cdr l)))))
    (cons first (sp l))))


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

(define (square-list items)
  (if (null? items)
      items
      (cons (square (car items))
	    (square-list (cdr items)))))

(define (square-list items)
  (map square items))

(define (for-each proc items)
  (cond ((null? items)
	 true)
	(else
	 (proc (car items))
	 (for-each proc (cdr items)))))

(for-each (lambda (x) (newline) (display x))
	  (map cube (list 1 2 3 4 5 6 7 8)))

(cadr (car (cddr (list 1 3 (list 5 7) 9))))
(caar (list (list 7)))
(cadr (cadr (cadr (list 1 (list 2 (list 3 7))))))

(define x (list 1 2 3))
(define y (list 4 5 6))

(define (deep-reverse l)
  (define (rev old new)
    (cond ((null? old)
	   new)
	  ((not (pair? old))
	   old)
	  (else
	   (rev (cdr old)
		(cons (rev (car old) (list))
		      new)))))
  (rev l (list))) ; (list) is the empty list

(define z (list (list 1 2) (list 3 4)))

; Remember, it's important to test whether an object is null before testing whether its a pair! Since the null object is considered not a pair.

(define (fringe t)
  (cond ((null? t)
	 (list)) ; (list) is the empty list
	((not (pair? t))
	 (list t))
	(else
	 (append (fringe (car t))
		 (fringe (cdr t))))))

					; Think about why this is correct. Interesting how you naturally think in terms of mathematical induction / structural induction - recursive functions are generally proved correct via mathematical induction! Also, notice how we used append and list structures in the implementation.

(define t1 (list 1 (list 2 (list 3 4) 5) 6))
(fringe (list z z))
(fringe (list (deep-reverse z) z))
(fringe t1)

(define (square-tree tree)
  (map (lambda (subtree)
	 (if (pair? subtree)
	     (square-tree subtree)
	     (square subtree)))
       tree))

(define (map-tree proc t)
  (define (mt t)
    (cond ((null? t)
	   (list))
	  ((not (pair? t))
	   (proc t))
	  (else
	   (cons (mt (car t))
		 (mt (cdr t))))))
  (mt t))
					; Map tree is a generalized version of map. It also works on regular list structures, as shown by the 4th use example below. Map is slightly more efficient for actual list-structures, however.

(map-tree square z)
(map-tree square (list z z))
(map-tree square t1)
(map-tree square (fringe t1))
