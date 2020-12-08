

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

(define (swap a b) ;; DOESN'T DO ANYTHING
  (let ((tmp a))
    (set! a b)
    (set! b tmp)))

;; Sqrt

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;; List processing

(define (equals? l1 l2)
  (cond ((and (pair? l1) (pair? l2))
	 (and (equal? (car l1) (car l2))
	      (equal? (cdr l1) (cdr l2))))
	((not (or (pair? l1) (pair? l2)))
	 (eq? l1 l2))
	(else false)))

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

(define (accumulate op init l)
  (if (null? l)
      init
      (op (car l) (accumulate op init (cdr l)))))

(define (length seq)
  (accumulate (lambda (x y) (+ y 1)) 0 seq))

;;;;;;
;;46
(define (make-semaphore n)
  (let ((count 0) (mutex (make-mutex)))
    (define (test-and-inc!)
      (mutex 'acquire)
      (if (= count n)
	  (begin
	    (mutex 'release)
	    true)
	  (begin
	    (set! count (+ count 1))
	    (mutex 'release)
	    false)))
      
    (define (acquire)
      (if (test-and-inc!)
	  (acquire)))
    
    (define (release)
      (mutex 'acquire)
      (set! count (- count 1))
      (mutex 'release))
    
    (define (me m)
      (cond ((eq? m 'acquire) (acquire))
	    ((eq? m 'release) (release))))
    me))
;; Note: We expect that the semaphore operations are used correctly - that is, if a procedure acquires a semaphore, it must release it at a later point. Abstracting semaphore usage away using serializers is a good way to ensure this is the case. Right now, there's a lot of areas for bad usage - e.g., using release without ever calling acquire.

;;47

(define nums 0)

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (make-account-and-serializer balance)
  (set! nums (+ nums 1))
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer))
	(account-num nums))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
	    ((eq? m 'account-num) account-num)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

(define (deposit account amount)
  (let ((s (account 'serializer))
        (d (account 'deposit)))
    ((s d) amount)))

(define (account-num account)
  (account 'account-num))

;; withdraw similarly defined

(define (serialized-exchange account1 account2)
  (let ((temp account2))
    (if (> (account-num account1) (account-num account2))
	(set! account2 account1)
	(set! account1 temp)))
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))

;; Testing stuff

(define a 4)
(define b 5)
(swap a b)

(define c (cons 1 2))
(define (modify a)
  (set-cdr! a 3))
(modify c)
;; Swap doesn't do anything! It swaps the values of the bindings in it's own environment (when we call swap, we create an environment with it's parameters set with the values of a and b. In other words, the expressions a and b are evaluated, and substituted in for the parameters of the procedure in the environment). However, these new bindings have no relation to the original a and b we called it with, the ones in the global environment. So we swap those bindings, exit the procedure call, and we observe that a and b are the exact same.

;; However, as we can see by running modify on c, we still can modify the value pointed to by the original name for the parameter.

;; In other words, seems like scheme is a little bit of pass by value, and pass by reference. We always refer to the same actual object (i.e., the place in memory with data) when we pass in an argument to a procedure. We evaluate the argument of the procedure call (be it an expression, a name, or whatever), and whatever object this is, we create a new environment and bind the object to the proper parameter names. Then we simply evaluate the actual procedure in this new environment.
