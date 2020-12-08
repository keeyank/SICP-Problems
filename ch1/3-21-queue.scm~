
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

;;17
(define empty-set '())

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((eq? x (car set)) true)
        (else (element-of-set? x (cdr set)))))
;; Note how we use eq? instead of equal?. We want to compare whether the literal object in memory has been encountered, and not use another definition for equality.

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (count-pairs x)
  (let ((encountered '()))
    (define (count-pairs-rec x)
      (cond ((not (pair? x)) 0)
	    ((element-of-set? x encountered) 0)
	    (else
	     (set! encountered
		   (adjoin-set x encountered))
	     (+ (count-pairs-rec (car x))
		(count-pairs-rec (cdr x))
		1))))
    (count-pairs-rec x)))

;; Note how the parameter x shadows the higher scope variable x.

(count-pairs (list 'a 'b 'c 'd 'e 'f))
(define l1 (list 'a 'b))
(count-pairs l1)
(define l2 (cons l1 l1))
(count-pairs l2)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define l3 (make-cycle l1))
(count-pairs l3)

;;18
;; Expects arguments to be a normal list.
(define (cycle? list)
  (let ((encountered empty-set))
    (define (cycle p)
      (cond ((null? p) #f)
	    ((element-of-set? p encountered) #t)
	    (else
	     (set! encountered
		   (adjoin-set p encountered))
	     (cycle (cdr p)))))
    (cycle list)))

(cycle? l3)
(cycle? (list 'a 'b 'c 'd 'e))
(cycle? (make-cycle (list 'a)))
(cycle? '())
