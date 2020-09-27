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



(define (make-interval l u)
  (if (< u l)
      (error "make-interval: upper < lower")
      (cons l u)))
(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (or (!= (sign (lower-bound y))
              (sign (upper-bound y)))
	  (= 0 (lower-bound y)))
      (error "div by zero")
      (mul-interval x
		    (make-interval (/ 1. (upper-bound y))
				   (/ 1. (lower-bound y))))))
					; We don't need to check if upper bounds equal 0, in this case the sign's will differ, or the interval is [0,0] and will be caught by lower-bound check.

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
		 (- (upper-bound x) (lower-bound y))))
					; Notice how everthing is basically pass-by-value - We are returning new intervals for every operation, which are really just the primitive pairs created using cons.



(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (make-center-width c (* c p)))

(define (percent i)
  (/ (width i) (center i)))

; Think of the layers of abstraction! From primite pairs to intervals, with constructor make-interval and selectors lower-bound and upper-bound. From intervals to center+width, with it's own selectors center and width, and constructor make-center-width. The selectors and constructors make use of the layer of abstraction preceding it. Then, the final layer - make-center-percent. This is Data Abstraction!!!!


(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
		(add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
		  (add-interval (div-interval one r1)
				(div-interval one r2)))))
					   
(define i1 (make-center-percent 50 .1))
(define i2 (make-center-percent 100 .01))
(define div-i1 (div-interval i1 i1)) ; Weird, this should be [1, 1]!!!! Since we know the actual number is somewhere in that interval, and we're diving the actual number in the interval by itself... But our program considers it has 2 separate intervals with 2 distinct numbers anywhere in those intervals.
(define div-i2 (div-interval i2 i1))

(define div-i1-cpy
  (make-center-percent (center div-i1)
		       (percent div-i1)))

(define div-i2-cpy
  (mul-interval i2
		(div-interval (make-interval 1 1)
			      i1)))

(define i3 (par1 i1 i2))
(define i4 (par2 i1 i2)) ; They're different!
					; This is due to shortcomings of interval arithmetic

