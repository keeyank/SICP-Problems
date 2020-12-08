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

;;7
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
	   (error "Incorrect password! "
		  pw))
	  ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

(define (make-joint account password1 password2)
  (define (withdraw amount)
    ((account 'withdraw password1) amount))
  (define (deposit amount)
    ((account 'deposit password1) amount))
  (define (dispatch m pw)
    (cond ((not (eq? pw password2))
	   (error "2nd account - Incorrect password! "
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

(define acc2 (make-joint acc 'pass 'word))
(acc2 'hello 'word)
(acc2 'hello 'wor)
((acc2 'deposit 'word) 100)
((acc2 'withdraw 'word) 200)

;; acc2 acts as an alias for acc, but it requires a different password. However, both essentially refer to the same "object".

(define acc3 (make-account 200 'pw))
((acc3 'withdraw 'pw) 20)

;;8
(define (f i)
  (define old 10)
  (define new 10)
  (set! old new)
  (set! new i)
  old)
(f 
