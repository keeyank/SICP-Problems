
(Define (square x) (* x x))
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

(define (equals? l1 l2)
  (cond ((and (pair? l1) (pair? l2))
	 (and (equal? (car l1) (car l2))
	      (equal? (cdr l1) (cdr l2))))
	((not (or (pair? l1) (pair? l2)))
	 (eq? l1 l2))
	(else false)))

;;24

(define (assoc key records same-key?)
  (cond ((null? records) false)
	((same-key? (caar records) key) (car records))
	(else (assoc key (cdr records) same-key?))))

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (lookup keys)
      (define (lookup-rec keys table)
	(if (null? keys)
	    (cdr table)	    
	    (let ((record (assoc (car keys) (cdr table) same-key?)))
	      (if record
		  (lookup-rec (cdr keys) record)
		  false))))
      (lookup-rec keys local-table))
    (define (insert! key value)
      (let ((record (assoc key (cdr local-table) same-key?)))
	(if record
            (set-cdr! record value)
            (set-cdr! local-table
                      (cons (cons key value) (cdr local-table)))))
      'ok) 
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define (insert! table key value)
  ((table 'insert-proc!) key value))
(define (lookup table key)
  ((table 'lookup-proc) key))


(define table (make-table (lambda (x y) (equals? x y))))
(insert! table 'a 3)
(insert! table 'a 4)
(insert! table 'b 3)
(insert! table 'c 'gogogo)
(lookup table (list 'a))
(lookup table (list 'b))
(lookup table (list 'c))

;;25
(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    
    (define (lookup keys)
      (define (lookup-rec keys table)
	(if (null? keys)
	    (cdr table)	    
	    (let ((record (assoc (car keys) (cdr table) same-key?)))
	      (if record
		  (lookup-rec (cdr keys) record)
		  false))))
      (lookup-rec keys local-table))

    (define (insert! keys val)
      
      (define (gen-record keys val)
	(if (null? (cdr keys))
	    (cons (car keys) val)
	    (cons (car keys)
		  (cons (gen-record (cdr keys) val)
			'()))))
	
      (define (insert!-rec keys val table)
	(if (null? keys)
	    (set-cdr! table val)
	    (let ((record (assoc (car keys) (cdr table) same-key?)))
	      (if record
		  (insert!-rec (cdr keys) val record)
		  (set-cdr! table (cons (gen-record keys val)
					(cdr table)))))))
			    
      (insert!-rec keys val local-table)
      'done)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    (else (error "Unknown operation -- TABLE" m))))
    
    dispatch))

(define (insert! table keys val)
  ((table 'insert-proc!) keys val))
(define (lookup table keys)
  ((table 'lookup-proc) keys))

(define table (make-table (lambda (x y) (equals? x y))))

(insert! table (list 'math '+) 8)
(lookup table (list 'math '+))
(insert! table (list 'math '+) 9)
(insert! table (list 'math '*) 2)
(insert! table (list 'math '/) 3)
(insert! table (list 'hi) 2)
(lookup table (list 'hi))
(lookup table (list 'math))
(insert! table (list 'math '% 'hi) 3)
(lookup table (list 'math '% 'hi))
(lookup table (list 'math))
(insert! table (list 'letters 'a) 3)
(lookup table (list 'letters 'a))
