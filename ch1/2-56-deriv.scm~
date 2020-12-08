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

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(list 'a 'b 'c)
(list (list 'george))
'((x1 x2) (y1 y2))
(cdr '((x1 x2) (y1 y2)))
(cadr '((x1 x2) (y1 y2)))
(caadr '((x1 x2) (y1 y2)))
(cadr (cadr '((x1 x2) (y1 y2))))
(pair? (car '(a short list)))
'(a short list)
(memq 'red '((red shoes) (blue socks)))
(memq 'red '(red shoes blue socks))

(define (equal? l1 l2)
  (cond ((and (pair? l1) (pair? l2))
	 (and (equal? (car l1) (car l2))
	      (equal? (cdr l1) (cdr l2))))
	((not (or (pair? l1) (pair? l2)))
	 (eq? l1 l2))
	(else false))) ; one is pair, other is symbol

;; Note how this implicitly deals with the empty list terminator: If one list ends before the other, the cdr of that list is not a pair (it's the empty list), while the cdr of the other list is a pair, so it goes into the third condition of the cond statement.
	
(define l1 '(a b c))
(define l2 (list 'a 'b 'c))
(define l3 '(a b c d))
(define nul '())
(define a 'a)
(define b 'b)
(define l4 '((x1 x2) (y1 y2)))
(define l5 (list (list 'x1 'x2) (list 'y1 'y2)))
(define l6 '((x1 x2) (y1 y2) z))

;; Try running equal? on some of these - it may help you figure out how the quote procedure works on nested lists.
