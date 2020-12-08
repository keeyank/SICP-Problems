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

;;1
(define (make-accum init)
  (lambda (addend)
    (set! init (+ init addend))
    init))

((make-accum 4) 3)
((make-accum 3) 10)
(define acc (make-accum 10))
(define acc2 (make-accum 10))
(acc 10)
(acc2 5)

;;2
(define (make-monitored f)
  (define count 0)
  (lambda (arg)
    (cond ((eq? 'how-many-calls? arg)
	   count)
	  ((eq? 'reset-count arg)
	   (set! count 0)
	   count)
	  (else
	   (set! count (+ 1 count))
	   (f arg)))))
;; Note how the lambda procedure is defined within the environment of f, so any calls to it will have access to count.

(define s (make-monitored square))
(s 'how-many-calls?)
(s 'reset-count)
(s 3)
;; Play around with C-c'ing these!


;;3
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m pw)
    (cond ((not (eq? pw password))
	   (error "Incorrect password!"
		  pw))
	  ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

(define acc (make-account 100 'pass))
((acc 'withdraw 'pass) 10)
((acc 'deposit 'pass) 12)
((acc 'deposit 'poo) 1)
((acc 'withdraw 'poo) 2)

;; note the similarities with the message passing procedures, local state, and C++s class system!
