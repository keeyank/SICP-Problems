(define (square x) (* x x))

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
			     
		       
	     	     
