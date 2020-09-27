(define (square x) (* x x))
(define (cube x) (* x x x))
(define (average a b) (/ (+ a b) 2))
(define (average3 a b c) (/ (+ a b c) 3))
(define (abs a) (if (< a 0) (- a) a))
(define (div a b) (truncate (/ a b)))
(define (inc n) (+ n 1))

(define (<= a b) (not (> a b)))
(define (>= a b) (not (< a b)))

(define (even? n) (= (remainder n 2) 0))



(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))

(define (fast-exp b n)
  (define (fast-exp-itr b n a)
    (cond ((= n 1) (* a b))
	  ((even? n) (fast-exp-itr (square b) (/ n 2) a))
	  (else (fast-exp-itr b (- n 1) (* a b)))))
  (fast-exp-itr b n 1))

(define (cons x y)
  (* (fast-exp 2 x) (fast-exp 3 y)))

(define (car x)
  (define (div-2 x n)
    (if (= (remainder x 2) 0)
	(div-2 (/ x 2) (+ n 1))
	n))
  (div-2 x 0))

(define (cdr x)
  (define (div-3 x n)
    (if (= (remainder x 3) 0)
	(div-3 (/ x 3) (+ n 1))
	n))
  (div-3 x 0))
