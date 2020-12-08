(define (square x) (* x x))
(define (cube x) (* x x x))
(define (average a b) (/ (+ a b) 2))
(define (average3 a b c) (/ (+ a b c) 3))
(define (abs a) (if (< a 0) (- a) a))
(define (div a b) (truncate (/ a b)))
(define (inc n) (+ n 1))

(define (exp b n)
  (define (fast-exp-itr b n a)
    (cond ((= n 1) (* a b))
	  ((even? n) (fast-exp-itr (square b) (/ n 2) a))
	  (else (fast-exp-itr b (- n 1) (* a b)))))
  (fast-exp-itr b n 1))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))


(define (deriv g)
  (define dx 0.000001)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newtons-method g guess)
  (define (newton-transform x)
    (- x (/ (g x) ((deriv g) x))))
  (fixed-point newton-transform guess))

(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
		  1.))

(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))


(define (double pro)
  (lambda (x)
    (pro (pro x))))
((double (lambda (x) (* x 2))) 1)
((double sqrt) 81)
((double sqrt) 16)
(((double (double double)) inc) 5)

(define (compose f g)
  (lambda (x)
    (f (g x))))
(define (square-then-inc) (compose inc square))
((square-then-inc) 2)
((compose square inc) 2)
((compose (lambda (x) (* x 3)) (lambda (x) (+ x 2))) 4)

(define (double pro) (compose pro pro))
  
(define (repeated f n)
  (if (= 1 n)
      f
      (lambda (x) (f ((repeated f (- n 1)) x)))))

(define (repeated f n)
  (if (= 1 n)
      f
      (compose f (repeated f (- n 1)))))
;; When reasoning about the correctness of repeated, feel free to use mathematical induction-like logic. Recursive procedures are always highly related to mathematical induction.

(define (smooth f)
  (define dx 0.000001)
  (lambda (x) (average3 (f (- x dx))
			(f x)
			(f (+ x dx)))))

					; Input to (repeated smooth n) is a function, making the repeated smooth function
(define (nfold-smooth f n)
  ((repeated smooth n) f))
;; Notice how the computation's here end up being exponential (big theta 3^n) because each repeated function call of f will call f 3 times, for each of the n layers.

(square 2)
((smooth square) 2)
((nfold-smooth square 5) 2)

(inc 2)
((smooth inc) 2)
((nfold-smooth inc 4) 2)

(define (iterative-improve good-enough? improve)
  (define (itr-imp x)
    (if (good-enough? x)
	x
	(itr-imp (improve x))))
  itr-imp) ; Return the function itr-imp
; Note: Brackets wouldn't work here! (itr-imp) makes the interpreter think that you are calling itr-imp with 0 arguments 

(define (fixed-point f first-guess)
  (define tolerance 0.000001)
  (define (good-enough? x)
    (< (abs (- x (f x))) tolerance))
  (define (improve x) (f x))
  ((iterative-improve good-enough? improve) first-guess))
    
;; 46
(define (nth-root x n)
  (fixed-point ((repeated average-damp
			  n) ; should be log base 2 of n
		(lambda (y)
		  (/ x (exp y (- n 1)))))
	       1.))

;; average-damp is a procedure that takes in a procedure. repeated also takes in a procedure, and returns a procedure which is the input procedure repeated n times. Since the input procedure, average-damp, takes as input another procedure, we pass that in to create the final multiple average-damped function that is used as input to the fixed poin function to find the nth-root.

;; For example, if we average damped the lambda function twice, it would basically be the function that applies average damping to the procedure twice in succession. That's what the first argument of fixed-point is.

;; It's hard to wrap your head around, but the gist is that functions can be treated just like any other data, and we can make really useful abstractions and automate certain things this way - in this case, we used the abstract repeated procedure to modify another procedure (average-damp) so that it applies a transformation on another function (our lambda procedures) multiple times. This lambda function is then used in the fixed-point procedure to find the fixed point of the function y -> y^(n-1), thus finding the n'th root. 
