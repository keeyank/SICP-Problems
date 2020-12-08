(define (square x) (* x x))
(define (cube x) (* x x x))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (accumulate combine base-val term next a b)
  (define (accum-itr a accum)
    (if (> a b)
	accum
	(accum-itr (next a) (combine accum (term a)))))
  (accum-itr a base-val))

(define (accumulate-rec combine base-val term next a b)
  (define (accum-rec a)
    (if (> a b)
	base-val
	(combine (term a) (accum-rec (next a)))))
  (accum-rec a))

(define (filter-acc combine pred base-val term next a b)
  (define (acc-itr curr accum)
    (if (> curr b)
	accum
        (if (pred curr)
	    (acc-itr (next curr)
		     (combine (term curr) accum))
	    (acc-itr (next curr) accum))))
  (acc-itr a base-val))

(define (sum-old term next a b)
  (define (sum-accum a accum)
    (if (< b a)
	accum
	(sum-accum (next a) (+ accum (term a)))))
  (sum-accum a 0.0))

(define (sum term next a b)
  (accumulate-rec (lambda (a b) (+ a b)) 0 term next a b))

(define (product-old term next a b)
  (define (prod-accum a accum)
    (if (< b a)
	accum
	(prod-accum (next a) (* accum (term a)))))
  (prod-accum a 1.0))

(define (product term next a b)
  (accumulate (lambda (a b) (* a b)) 1 term next a b))

(define (product-rec term next a b)
  (if (> a b)
      1
      (* (term a)
	 (product-rec term next (next a) b))))
    

(define (fact n)
  (product (lambda (x) x) (lambda (x) (+ x 1)) 1 n))

(define pi (* (product-old (lambda (x) (/ x (+ x 1)))
		       (lambda (x) (+ x 2))
		       2
		       10000)
	      (product-old (lambda (x) (/ x (- x 1)))
		       (lambda (x) (+ x 2))
		       4
		       10000)
	      4))

;; 29
(define (simpsons f a b n)
  (define h (/ (- b a) n))
  (define (next x)
    (+ x (* 2 h)))
  
  (* (/ h 3)
     (+ (* 4
	   (sum f
		next
		(+ a h)
		(+ a (* (- n 1) h))))
	(* 2
	   (sum f
		next
		(+ a (* 2 h))
		(+ a (* (- n 2) h))))
	(f a)
	(f (+ a (* n h))))))

;; 33
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

(define (sum-of-square-primes a b)
  (define (next n) (+ n 1))
  (filter-acc + prime? 0 square next a b))

(define (prod-of-relatively-prime n)
  (define (identity n) n)
  (define (next n) (+ n 1))
  (define (relative-prime? m)
    (= 1 (gcd n m)))
  (filter-acc * relative-prime? 1 identity next 1 n))
    
		       
